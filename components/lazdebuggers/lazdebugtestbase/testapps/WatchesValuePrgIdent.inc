
  pre__Byte          _OP_ Byte        (1                    + ADD);
  pre__Word          _OP_ Word        (100                  + ADD);
  pre__Longword      _OP_ Longword    (1000                 + ADD);
  pre__QWord         _OP_ QWord       (10000                + ADD);
  pre__Shortint      _OP_ Shortint    (50                   + ADD);
  pre__Smallint      _OP_ Smallint    (500                  + ADD);
  pre__Longint       _OP_ Longint     (5000                 + ADD);
  pre__Int64         _OP_ Int64       (50000                + ADD);

  pre__Byte_2        _OP_ Byte        (250                  + ADD);
  pre__Word_2        _OP_ Word        (65501                + ADD);
  pre__Longword_2    _OP_ Longword    (4123456789           + ADD);
  pre__QWord_2       _OP_ QWord       (15446744073709551610 + ADD);
  pre__Shortint_2    _OP_ Shortint    (122                  + ADD);
  pre__Smallint_2    _OP_ Smallint    (32012                + ADD);
  pre__Longint_2     _OP_ Longint     (20123456             + ADD);
  pre__Int64_2       _OP_ Int64       (9123372036854775801  + ADD);

  pre__Shortint_3    _OP_ Shortint    (-122                 + ADD);
  pre__Smallint_3    _OP_ Smallint    (-32012               + ADD);
  pre__Longint_3     _OP_ Longint     (-20123456            + ADD);
  pre__Int64_3       _OP_ Int64       (-9123372036854775801 + ADD);

  pre__Real          _OP_ Real        (50.25                + ADD);
  pre__Single        _OP_ Single      (100.125              + ADD);
  pre__Double        _OP_ Double      (1000.125             + ADD);
  pre__Extended      _OP_ Extended    (10000.175            + ADD);
  //pre__Comp        _OP_ Comp        (50.125               + ADD);
  pre__Currency      _OP_ Currency    (125.123              + ADD);

  pre__Real_2        _OP_ Real        (-50.25               + ADD);
  pre__Single_2      _OP_ Single      (-100.125             + ADD);
  pre__Double_2      _OP_ Double      (-1000.125            + ADD);
  pre__Extended_2    _OP_ Extended    (-10000.175           + ADD);
  //pre__Comp_2      _OP_ Comp        (-150.125             + ADD);
  pre__Currency_2    _OP_ Currency    (-125.123             + ADD);

  pre__Char          _OP_ char        (CHR1                    );
  pre__Char2         _OP_ char        (#0                      );
  pre__Char3         _OP_ char        (' '                     );

  pre__String1       _OP_ ShortStr1   (CHR1+''                 );
  pre__String1e      _OP_ ShortStr1   (     ''                 );
  pre__String10      _OP_ ShortStr10  (CHR1+'bc1'              );
  pre__String10e     _OP_ ShortStr10  (     ''                 );
  pre__String10x     _OP_ ShortStr10  (CHR1+'S'#0'B'#9'b'#10#13);
  pre__String255     _OP_ ShortStr255 (CHR1+'bcd0123456789'    );
  pre__Ansi1         _OP_ AnsiString  (succ(CHR1)              );
  pre__Ansi2         _OP_ AnsiString  (CHR1+'abcd0123'         );
  pre__Ansi3         _OP_ AnsiString  (     ''                 );
  pre__Ansi4         _OP_ TStrA       (CHR1+'A'#0'B'#9'b'#10#13);
  pre__Ansi5         _OP_ AnsiString  (CHR1+'bcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij');

  pre__PChar         _OP_ PChar       ( nil );
  pre__PChar2        _OP_ TPChr       ( nil );

  pre__WideChar      _OP_ char        (CHR1                    );
  pre__WideChar2     _OP_ char        (#0                      );
  pre__WideChar3     _OP_ char        (' '                     );

  pre__WideString1   _OP_ WideString  (succ(CHR1)              );
  pre__WideString2   _OP_ WideString  (CHR1+'abcX0123'         );
  pre__WideString3   _OP_ WideString  (     ''                 );
  pre__WideString4   _OP_ TWStrA      (CHR1+'A'#0'X'#9'b'#10#13);
  pre__WideString5   _OP_ TWStrTA     (CHR1+'XcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij');

  pre__PWideChar     _OP_ PWideChar   ( nil );
  pre__PWideChar2    _OP_ TPWChr      ( nil );

// wide string char...

  // types that may get confused with strings

  pre__ShortRec       _O2_ TShortRec               _EQ_ {P2} (length: 5; st:  (CHR1, 'b',CHR1, 'b','c')); //_pre2_ShortRec; pre__ShortRec.st[1] := CHR1; pre__ShortRec.st[3] := CHR1;
  pre__CharDynArray   _O2_ array of char           _EQ_      (nil);
  pre__WCharDynArray  _O2_ array of widechar       _EQ_      (nil);

//ARG{ Not allowed in argument list
  pre__CharStatArray  _O2_ array[1..5] of char     _EQ_ {P2} (CHR1, 'b',CHR1, 'B','c'); //_pre2_CharStatArray; pre__CharStatArray[1]:=CHR1; pre__CharStatArray[3]:=CHR1;
  pre__WCharStatArray _O2_ array[1..5] of widechar _EQ_ {P2} (CHR1, 'b',CHR1, 'B','c'); //_pre2_WCharStatArray; pre__WCharStatArray[1]:=CHR1; pre__WCharStatArray[3]:=CHR1;
//}

// subset
// enum
// set
// array dyn/stat
// record
// object
// class
