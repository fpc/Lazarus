{$DEFINE _BLOCK_}
{$DEFINE _BLOCK2_}

// ADD max 15

  pre__Byte{e}          _OP_ Byte        (1                    + ADD);          //@@ _pre3_Byte;
  pre__Word{e}          _OP_ Word        (100                  + ADD);          //@@ _pre3_Word;
  pre__Longword{e}      _OP_ Longword    (1000                 + ADD);          //@@ _pre3_Longword;
  pre__QWord{e}         _OP_ QWord       (10000                + ADD);          //@@ _pre3_QWord;
  pre__Shortint{e}      _OP_ Shortint    (50                   + ADD);          //@@ _pre3_Shortint;
  pre__Smallint{e}      _OP_ Smallint    (500                  + ADD);          //@@ _pre3_Smallint;
  pre__Longint{e}       _OP_ Longint     (5000                 + ADD);          //@@ _pre3_Longint;
  pre__Int64{e}         _OP_ Int64       (50000                + ADD);          //@@ _pre3_Int64;
  pre__IntRange{e}      _OP_ TIntRange   (-50                  + ADD);          //@@ _pre3_IntRange;
  pre__CardinalRange{e} _OP_ TCardinalRange(50                  + ADD);         //@@ _pre3_CardinalRange;

  pre__Byte_2{e}        _OP_ Byte        (240                  + ADD);          //@@ _pre3_Byte_2;
  pre__Word_2{e}        _OP_ Word        (65501                + ADD);          //@@ _pre3_Word_2;
  pre__Longword_2{e}    _OP_ Longword    (4123456789           + ADD);          //@@ _pre3_Longword_2;
  pre__QWord_2{e}       _OP_ QWord       (15446744073709551610 + ADD);          //@@ _pre3_QWord_2;
  pre__Shortint_2{e}    _OP_ Shortint    (112                  + ADD);          //@@ _pre3_Shortint_2;
  pre__Smallint_2{e}    _OP_ Smallint    (32012                + ADD);          //@@ _pre3_Smallint_2;
  pre__Longint_2{e}     _OP_ Longint     (20123456             + ADD);          //@@ _pre3_Longint_2;
  pre__Int64_2{e}       _OP_ Int64       (9123372036854775801  + ADD);          //@@ _pre3_Int64_2;

  pre__Shortint_3{e}    _OP_ Shortint    (-112                 + ADD);          //@@ _pre3_Shortint_3;
  pre__Smallint_3{e}    _OP_ Smallint    (-32012               + ADD);          //@@ _pre3_Smallint_3;
  pre__Longint_3{e}     _OP_ Longint     (-20123456            + ADD);          //@@ _pre3_Longint_3;
  pre__Int64_3{e}       _OP_ Int64       (-9123372036854775801 + ADD);          //@@ _pre3_Int64_3;

  pre__Real{e}          _OP_ Real        (50.25                + ADD);          //@@ _pre3_Real;
  pre__Single{e}        _OP_ Single      (100.125              + ADD);          //@@ _pre3_Single;
  pre__Double{e}        _OP_ Double      (1000.125             + ADD);          //@@ _pre3_Double;
  pre__Extended{e}      _OP_ Extended    (10000.175            + ADD);          //@@ _pre3_Extended;
  //pre__Comp{e}        _OP_ Comp        (50.125               + ADD);          //@@ //_pre3_Comp;
  pre__Currency{e}      _OP_ Currency    (125.123              + ADD);          //@@ _pre3_Currency;

  pre__Real_2{e}        _OP_ Real        (-50.25               + ADD);          //@@ _pre3_Real_2;
  pre__Single_2{e}      _OP_ Single      (-100.125             + ADD);          //@@ _pre3_Single_2;
  pre__Double_2{e}      _OP_ Double      (-1000.125            + ADD);          //@@ _pre3_Double_2;
  pre__Extended_2{e}    _OP_ Extended    (-10000.175           + ADD);          //@@ _pre3_Extended_2;
  //pre__Comp_2{e}      _OP_ Comp        (-150.125             + ADD);          //@@ //_pre3_Comp_2;
  pre__Currency_2{e}    _OP_ Currency    (-125.123             + ADD);          //@@ _pre3_Currency_2;

  // **** Char and String types ****

  pre__Char{e}          _OP_ char        (CHR1                    );            //@@ _pre3_Char;
  pre__Char2{e}         _OP_ char        (#0                      );            //@@ _pre3_Char2;
  pre__Char3{e}         _OP_ char        (' '                     );            //@@ _pre3_Char3;

  pre__String1{e}       _OP_ ShortStr1   (CHR1+''                 );            //@@ _pre3_String1;
  pre__String1e{e}      _OP_ ShortStr1   (     ''                 );            //@@ _pre3_String1e;
  pre__String10{e}      _OP_ ShortStr10  (CHR1+'bc1'              );            //@@ _pre3_String10;
  pre__String10e{e}     _OP_ ShortStr10  (     ''                 );            //@@ _pre3_String10e;
  pre__String10x{e}     _OP_ ShortStr10  (CHR1+'S'#0'B'#9'b'#10#13);            //@@ _pre3_String10x;
  pre__String255{e}     _OP_ ShortStr255 (CHR1+'bcd0123456789'    );            //@@ _pre3_String255;
  pre__Ansi1{e}         _OP_ AnsiString  (succ(CHR1)              );            //@@ _pre3_Ansi1;
  pre__Ansi2{e}         _OP_ AnsiString  (CHR1+'abcd0123'         );            //@@ _pre3_Ansi2;
  pre__Ansi3{e}         _OP_ AnsiString  (     ''                 );            //@@ _pre3_Ansi3;
  pre__Ansi4{e}         _OP_ TStrA       (CHR1+'A'#0'B'#9'b'#10#13);            //@@ _pre3_Ansi4;
  pre__Ansi5{e}         _OP_ AnsiString  (CHR1+'bcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij');          //@@ _pre3_Ansi5;

  pre__PChar{e}         _OP_ PChar       ( nil );                               //@@ _pre3_PChar;
  pre__PChar2{e}        _OP_ TPChr       ( nil );                               //@@ _pre3_PChar2;
  {$IFDEF TestAssign}
  pre__PChar2{e} := @pre__Ansi2{e}[1];                                          //@@ _pre3_PChar2; // }
  {$ENDIF}

  pre__WideChar{e}      _OP_ char        (CHR1                    );            //@@ _pre3_WideChar;
  pre__WideChar2{e}     _OP_ char        (#0                      );            //@@ _pre3_WideChar2;
  pre__WideChar3{e}     _OP_ char        (' '                     );            //@@ _pre3_WideChar3;

  pre__WideString1{e}   _OP_ WideString  (succ(CHR1)              );            //@@ _pre3_WideString1;
  pre__WideString2{e}   _OP_ WideString  (CHR1+'abcX0123'         );            //@@ _pre3_WideString2;
  pre__WideString3{e}   _OP_ WideString  (     ''                 );            //@@ _pre3_WideString3;
  pre__WideString4{e}   _OP_ TWStrA      (CHR1+'A'#0'X'#9'b'#10#13);            //@@ _pre3_WideString4;
  pre__WideString5{e}   _OP_ TWStrTA     (CHR1+'XcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij');          //@@ _pre3_WideString5;

  pre__PWideChar{e}     _OP_ PWideChar   ( nil );                               //@@ _pre3_PWideChar;
  pre__PWideChar2{e}    _OP_ TPWChr      ( nil );                               //@@ _pre3_PWideChar2;
  {$IFDEF TestAssign}
  pre__PWideChar2{e} := @pre__WideString2{e}[1];                                //@@ _pre3_PWideChar2; // }
  {$ENDIF}

  pre__UnicodeString1{e}   _OP_ UnicodeString  (succ(CHR1)              );      //@@ _pre3_UnicodeString1;
  pre__UnicodeString2{e}   _OP_ UnicodeString  (CHR1+'aBcX0123'         );      //@@ _pre3_UnicodeString2;
  pre__UnicodeString3{e}   _OP_ UnicodeString  (     ''                 );      //@@ _pre3_UnicodeString3;
  pre__UnicodeString4{e}   _OP_ TUStrA      (CHR1+'B'#0'X'#9'b'#10#13);         //@@ _pre3_UnicodeString4;
  pre__UnicodeString5{e}   _OP_ TUStrTA     (CHR1+'YcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij');          //@@ _pre3_UnicodeString5;

// wide string char...

  // types that may get confused with strings

  {$IFnDEF TestAssign}
  pre__ShortRec{e}       _O2_ TShortRec               _EQ_      (length: 5; st:  (CHR1, 'b',CHR1, 'b','c'));       //@@ _pre3_ShortRec;
  {$ELSE}
  pre__ShortRec{e}       := _pre2_ShortRec;   pre__ShortRec{e}.st[1] := CHR1; pre__ShortRec{e}.st[3] := CHR1;      //@@ _pre3_ShortRec; // }}
  {$ENDIF}

  // **** ARRAY ****
  // **** Dyn ARRAY ****

  {$IFnDEF TestPointer}
  pre__CharDynArray{e}    _O2_ array of char          _EQ_      (nil);          //@@ _pre3_CharDynArray;  // open array if used as function arg;
  pre__CharDynArray2{e}   _O2_ array of char          _EQ_      (nil);          //@@ _pre3_CharDynArray2; // len = 3 // open array if used as function arg;
  {$ENDIF} // TestPointer
  pre__CharDynArray3{e}   _O2_ TCharDynArray          _EQ_      (nil);          //@@ _pre3_CharDynArray3;
  pre__CharDynArray4{e}   _O2_ TCharDynArray          _EQ_      (nil);          //@@ _pre3_CharDynArray4; // len = 3;

  {$IFnDEF TestPointer}
  pre__WCharDynArray{e}   _O2_ array of widechar      _EQ_      (nil);          //@@ _pre3_WCharDynArray;
  pre__WCharDynArray2{e}  _O2_ array of widechar      _EQ_      (nil);          //@@ _pre3_WCharDynArray2; // len = 3;
  {$ENDIF} // TestPointer
  pre__WCharDynArray3{e}  _O2_ TWCharDynArray         _EQ_      (nil);          //@@ _pre3_WCharDynArray3;
  pre__WCharDynArray4{e}  _O2_ TWCharDynArray         _EQ_      (nil);          //@@ _pre3_WCharDynArray4; // len = 3;

  {$IFnDEF TestPointer}
  pre__IntDynArray{e}    _O2_ array of Integer        _EQ_      (nil);          //@@ _pre3_IntDynArray;    // open array if used as function arg;
  pre__IntDynArray2{e}   _O2_ array of Integer        _EQ_      (nil);          //@@ _pre3_IntDynArray2;   // len = 3 // open array if used as function arg;
  {$ENDIF} // TestPointer
  pre__IntDynArray3{e}   _O2_ TIntDynArray            _EQ_      (nil);          //@@ _pre3_IntDynArray3;
  pre__IntDynArray4{e}   _O2_ TIntDynArray            _EQ_      (nil);          //@@ _pre3_IntDynArray4;   // len = 3;

  pre__IntDynArray5{e}   _OP_ TIntDynArray ( nil );                             //@@ _pre3_IntDynArray5;   // REAL CONST = nil

  {$IFnDEF TestPointer}
  pre__AnsiDynArray{e}    _O2_ array of AnsiString    _EQ_      (nil);          //@@ _pre3_AnsiDynArray;    // open array if used as function arg;
  pre__AnsiDynArray2{e}   _O2_ array of AnsiString    _EQ_      (nil);          //@@ _pre3_AnsiDynArray2;   // len = 3 // open array if used as function arg;
  {$ENDIF} // TestPointer
  pre__AnsiDynArray3{e}   _O2_ TAnsiDynArray          _EQ_      (nil);          //@@ _pre3_AnsiDynArray3;
  pre__AnsiDynArray4{e}   _O2_ TAnsiDynArray          _EQ_      (nil);          //@@ _pre3_AnsiDynArray4;   // len = 3;

  {$IFnDEF TestPointer}
  pre__ShortStrDynArray{e}    _O2_ array of ShortStr10 _EQ_      (nil);         //@@ _pre3_ShortStrDynArray;    // open array if used as function arg;
  pre__ShortStrDynArray2{e}   _O2_ array of ShortStr10 _EQ_      (nil);         //@@ _pre3_ShortStrDynArray2;   // len = 3 // open array if used as function arg;
  {$ENDIF} // TestPointer
  pre__ShortStrDynArray3{e}   _O2_ TShortStrDynArray   _EQ_      (nil);         //@@ _pre3_ShortStrDynArray3;
  pre__ShortStrDynArray4{e}   _O2_ TShortStrDynArray   _EQ_      (nil);         //@@ _pre3_ShortStrDynArray4;   // len = 3;

  pre__DynDynArrayInt{e}      _O2_ TDynDynArrayInt     _EQ_      (nil);         //@@ _pre3_DynDynArrayInt;
  pre__DynDynArrayInt2{e}     _O2_ TDynDynArrayInt     _EQ_      (nil);         //@@ _pre3_DynDynArrayInt2;

  pre__FiveDynArray{e}             _O2_ TFiveDynArray            _EQ_ (nil);    //@@ _pre3_FiveDynArray;
  pre__FiveDynArrayPack{e}         _O2_ TFiveDynArrayPack        _EQ_ (nil);    //@@ _pre3_FiveDynArrayPack;
  pre__FivePackDynArray{e}         _O2_ TFivePackDynArray        _EQ_ (nil);    //@@ _pre3_FivePackDynArray;
  pre__FivePackDynArrayPack{e}     _O2_ TFivePackDynArrayPack    _EQ_ (nil);    //@@ _pre3_FivePackDynArrayPack;
  pre__RecFiveDynArray{e}          _O2_ TRecFiveDynArray         _EQ_ (nil);    //@@ _pre3_RecFiveDynArray;
  pre__RecFiveDynPackArray{e}      _O2_ TRecFiveDynPackArray     _EQ_ (nil);    //@@ _pre3_RecFiveDynPackArray;
  pre__RecFivePackDynArray{e}      _O2_ TRecFivePackDynArray     _EQ_ (nil);    //@@ _pre3_RecFivePackDynArray;
  pre__RecFivePackDynPackArray{e}  _O2_ TRecFivePackDynPackArray _EQ_ (nil);    //@@ _pre3_RecFivePackDynPackArray;
{$IFnDEF TestAssignGC}
{$IFnDEF TestParam}
{$IFnDEF TestArg}
  {$IFnDEF TestPointer}
  pre__FiveDynArray2{e}            _O2_        array of          record a:longint; b: byte end _EQ_ (nil);          //@@ _pre3_FiveDynArray2;
  pre__FiveDynArrayPack2{e}        _O2_ packed array of          record a:longint; b: byte end _EQ_ (nil);          //@@ _pre3_FiveDynArrayPack2;
  pre__FivePackDynArray2{e}        _O2_        array of   packed record a:longint; b: byte end _EQ_ (nil);          //@@ _pre3_FivePackDynArray2;
  pre__FivePackDynArrayPack2{e}    _O2_ packed array of   packed record a:longint; b: byte end _EQ_ (nil);          //@@ _pre3_FivePackDynArrayPack2;
  {$ENDIF} // TestPointer
{$ENDIF}
{$ENDIF}
{$ENDIF}

  {$IFDEF TestAssign}
  SetLength(pre__CharDynArray2    {e}, 3);    pre__CharDynArray2{e}[0]     _OP_ 'N';          //@@ // }
                                              pre__CharDynArray2{e}[1]     _OP_ CHR1;         //@@
                                              pre__CharDynArray2{e}[2]     _OP_ 'M';          //@@
  SetLength(pre__CharDynArray4    {e}, 3);    pre__CharDynArray4{e}[0]     _OP_ 'J';          //@@ // }
                                              pre__CharDynArray4{e}[1]     _OP_ CHR1;         //@@
                                              pre__CharDynArray4{e}[2]     _OP_ 'M';          //@@

  SetLength(pre__WCharDynArray2   {e}, 3);    pre__WCharDynArray2{e}[0]    _OP_ 'W';          //@@ // }
                                              pre__WCharDynArray2{e}[1]    _OP_ CHR1;         //@@
                                              pre__WCharDynArray2{e}[2]    _OP_ 'M';          //@@
  SetLength(pre__WCharDynArray4   {e}, 3);    pre__WCharDynArray4{e}[0]    _OP_ 'K';          //@@ // }
                                              pre__WCharDynArray4{e}[1]    _OP_ CHR1;         //@@
                                              pre__WCharDynArray4{e}[2]    _OP_ 'M';          //@@

  SetLength(pre__IntDynArray2     {e}, 3);    pre__IntDynArray2{e}[0]      _OP_ 11;         //@@ // }
                                              pre__IntDynArray2{e}[1]      _OP_ 30+ADD;         //@@
                                              pre__IntDynArray2{e}[2]      _OP_ 60;         //@@
  SetLength(pre__IntDynArray4     {e}, 3);    pre__IntDynArray4{e}[0]      _OP_ 12;         //@@ // }
                                              pre__IntDynArray4{e}[1]      _OP_ 30+ADD;         //@@
                                              pre__IntDynArray4{e}[2]      _OP_ 60;         //@@

  SetLength(pre__AnsiDynArray2    {e}, 3);    pre__AnsiDynArray2{e}[0]     _OP_ 'N123';         //@@ // }
                                              pre__AnsiDynArray2{e}[1]     _OP_ CHR1+'ab';         //@@
                                              pre__AnsiDynArray2{e}[2]     _OP_ 'M'#9;             //@@
  SetLength(pre__AnsiDynArray4    {e}, 3);    pre__AnsiDynArray4{e}[0]     _OP_ 'J123';            //@@ // }
                                              pre__AnsiDynArray4{e}[1]     _OP_ CHR1+'ab';         //@@
                                              pre__AnsiDynArray4{e}[2]     _OP_ 'M'#9;             //@@

  SetLength(pre__ShortStrDynArray2{e}, 3);    pre__ShortStrDynArray2{e}[0] _OP_ 'N123';            //@@ // }
                                              pre__ShortStrDynArray2{e}[1] _OP_ CHR1+'ac';         //@@
                                              pre__ShortStrDynArray2{e}[2] _OP_ 'M'#9;             //@@
  SetLength(pre__ShortStrDynArray4{e}, 3);    pre__ShortStrDynArray4{e}[0] _OP_ 'J123';            //@@ // }
                                              pre__ShortStrDynArray4{e}[1] _OP_ CHR1+'ac';         //@@
                                              pre__ShortStrDynArray4{e}[2] _OP_ 'M'#9;             //@@

  SetLength(pre__DynDynArrayInt{e}, 5); // }
  SetLength(pre__DynDynArrayInt{e}[0], 3);    pre__DynDynArrayInt{e}[0][0] := 11+ADD; // }}
                                              pre__DynDynArrayInt{e}[0][1] := 0; // }
                                              pre__DynDynArrayInt{e}[0][2] := -22; // }
  SetLength(pre__DynDynArrayInt{e}[1], 1);    pre__DynDynArrayInt{e}[1][0] := 110+ADD; // }}
            pre__DynDynArrayInt{e}[2]  := pre__DynDynArrayInt{e}[0]; // }}
  SetLength(pre__DynDynArrayInt{e}[3], 0); // }
  SetLength(pre__DynDynArrayInt{e}[4], 4);    pre__DynDynArrayInt{e}[4][0] := 11; // }}
                                              pre__DynDynArrayInt{e}[4][1] := 12; // }
                                              pre__DynDynArrayInt{e}[4][2] := 11; // }
                                              pre__DynDynArrayInt{e}[4][3] := 10; // }
  pre__DynDynArrayInt2 := pre__DynDynArrayInt;

  SetLength(pre__FiveDynArray           {e},3); pre__FiveDynArray           {e}[0].a _OP_ -1-ADD; pre__FiveDynArray           {e}[0].b _OP_ 11; // }}}
                                                pre__FiveDynArray           {e}[1].a _OP_ -2-ADD; pre__FiveDynArray           {e}[1].b _OP_ 22; // }}}
                                                pre__FiveDynArray           {e}[2].a _OP_ -3-ADD; pre__FiveDynArray           {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FiveDynArrayPack       {e},3); pre__FiveDynArrayPack       {e}[0].a _OP_ -1-ADD; pre__FiveDynArrayPack       {e}[0].b _OP_ 11; // }}}
                                                pre__FiveDynArrayPack       {e}[1].a _OP_ -2-ADD; pre__FiveDynArrayPack       {e}[1].b _OP_ 22; // }}}
                                                pre__FiveDynArrayPack       {e}[2].a _OP_ -3-ADD; pre__FiveDynArrayPack       {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FivePackDynArray       {e},3); pre__FivePackDynArray       {e}[0].a _OP_ -1-ADD; pre__FivePackDynArray       {e}[0].b _OP_ 11; // }}}
                                                pre__FivePackDynArray       {e}[1].a _OP_ -2-ADD; pre__FivePackDynArray       {e}[1].b _OP_ 22; // }}}
                                                pre__FivePackDynArray       {e}[2].a _OP_ -3-ADD; pre__FivePackDynArray       {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FivePackDynArrayPack   {e},3); pre__FivePackDynArrayPack   {e}[0].a _OP_ -1-ADD; pre__FivePackDynArrayPack   {e}[0].b _OP_ 11; // }}}
                                                pre__FivePackDynArrayPack   {e}[1].a _OP_ -2-ADD; pre__FivePackDynArrayPack   {e}[1].b _OP_ 22; // }}}
                                                pre__FivePackDynArrayPack   {e}[2].a _OP_ -3-ADD; pre__FivePackDynArrayPack   {e}[2].b _OP_ 33; // }}}
  SetLength(pre__RecFiveDynArray        {e},3); pre__RecFiveDynArray        {e}[0].a _OP_ -1-ADD; pre__RecFiveDynArray        {e}[0].b _OP_ 11; // }}}
                                                pre__RecFiveDynArray        {e}[1].a _OP_ -2-ADD; pre__RecFiveDynArray        {e}[1].b _OP_ 22; // }}}
                                                pre__RecFiveDynArray        {e}[2].a _OP_ -3-ADD; pre__RecFiveDynArray        {e}[2].b _OP_ 33; // }}}
  SetLength(pre__RecFiveDynPackArray    {e},3); pre__RecFiveDynPackArray    {e}[0].a _OP_ -1-ADD; pre__RecFiveDynPackArray    {e}[0].b _OP_ 11; // }}}
                                                pre__RecFiveDynPackArray    {e}[1].a _OP_ -2-ADD; pre__RecFiveDynPackArray    {e}[1].b _OP_ 22; // }}}
                                                pre__RecFiveDynPackArray    {e}[2].a _OP_ -3-ADD; pre__RecFiveDynPackArray    {e}[2].b _OP_ 33; // }}}
  SetLength(pre__RecFivePackDynArray    {e},3); pre__RecFivePackDynArray    {e}[0].a _OP_ -1-ADD; pre__RecFivePackDynArray    {e}[0].b _OP_ 11; // }}}
                                                pre__RecFivePackDynArray    {e}[1].a _OP_ -2-ADD; pre__RecFivePackDynArray    {e}[1].b _OP_ 22; // }}}
                                                pre__RecFivePackDynArray    {e}[2].a _OP_ -3-ADD; pre__RecFivePackDynArray    {e}[2].b _OP_ 33; // }}}
  SetLength(pre__RecFivePackDynPackArray{e},3); pre__RecFivePackDynPackArray{e}[0].a _OP_ -1-ADD; pre__RecFivePackDynPackArray{e}[0].b _OP_ 11; // }}}
                                                pre__RecFivePackDynPackArray{e}[1].a _OP_ -2-ADD; pre__RecFivePackDynPackArray{e}[1].b _OP_ 22; // }}}
                                                pre__RecFivePackDynPackArray{e}[2].a _OP_ -3-ADD; pre__RecFivePackDynPackArray{e}[2].b _OP_ 33; // }}}
  SetLength(pre__FiveDynArray2          {e},3); pre__FiveDynArray2          {e}[0].a _OP_ -1-ADD; pre__FiveDynArray2          {e}[0].b _OP_ 11; // }}}
                                                pre__FiveDynArray2          {e}[1].a _OP_ -2-ADD; pre__FiveDynArray2          {e}[1].b _OP_ 22; // }}}
                                                pre__FiveDynArray2          {e}[2].a _OP_ -3-ADD; pre__FiveDynArray2          {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FiveDynArrayPack2      {e},3); pre__FiveDynArrayPack2      {e}[0].a _OP_ -1-ADD; pre__FiveDynArrayPack2      {e}[0].b _OP_ 11; // }}}
                                                pre__FiveDynArrayPack2      {e}[1].a _OP_ -2-ADD; pre__FiveDynArrayPack2      {e}[1].b _OP_ 22; // }}}
                                                pre__FiveDynArrayPack2      {e}[2].a _OP_ -3-ADD; pre__FiveDynArrayPack2      {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FivePackDynArray2      {e},3); pre__FivePackDynArray2      {e}[0].a _OP_ -1-ADD; pre__FivePackDynArray2      {e}[0].b _OP_ 11; // }}}
                                                pre__FivePackDynArray2      {e}[1].a _OP_ -2-ADD; pre__FivePackDynArray2      {e}[1].b _OP_ 22; // }}}
                                                pre__FivePackDynArray2      {e}[2].a _OP_ -3-ADD; pre__FivePackDynArray2      {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FivePackDynArrayPack2  {e},3); pre__FivePackDynArrayPack2  {e}[0].a _OP_ -1-ADD; pre__FivePackDynArrayPack2  {e}[0].b _OP_ 11; // }}}
                                                pre__FivePackDynArrayPack2  {e}[1].a _OP_ -2-ADD; pre__FivePackDynArrayPack2  {e}[1].b _OP_ 22; // }}}
                                                pre__FivePackDynArrayPack2  {e}[2].a _OP_ -3-ADD; pre__FivePackDynArrayPack2  {e}[2].b _OP_ 33; // }}}
  {$ENDIF}

  // **** Static ARRAY ****
{$IFnDEF TestParam}
{$IFnDEF TestArg}
  {$IFnDEF TestPointer}
  {$IFnDEF TestAssign}
  pre__CharStatArray{e}     _O2_ array[1..5] of char       _EQ_      (CHR1, 'b',CHR1, 'B','c');                //@@ _pre3_CharStatArray;
  pre__WCharStatArray{e}    _O2_ array[1..5] of widechar   _EQ_      (CHR1, 'b',CHR1, 'B','d');                //@@ _pre3_WCharStatArray;
  pre__IntStatArray{e}      _O2_ array[1..5] of Integer    _EQ_      (-1, 300+ADD, 2, 0, 1);                   //@@ _pre3_IntStatArray;
  pre__AnsiStatArray{e}     _O2_ array[1..5] of AnsiString _EQ_      (CHR1, 'b123',CHR1+'ab', 'B','cdef'#9);   //@@ _pre3_AnsiStatArray;
  pre__ShortStrStatArray{e} _O2_ array[1..5] of ShortStr10 _EQ_      (CHR1, 'b123',CHR1+'ab', 'C','cdef'#9);   //@@ _pre3_ShortStrStatArray;

//  pre__FiveStatArray2{e}         _O2_        array [2..4] of        record a:longint; b: byte end _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArray2;
//  pre__FiveStatArrayPack2{e}     _O2_ packed array [2..4] of        record a:longint; b: byte end _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArrayPack2;
//  pre__FivePackStatArray2{e}     _O2_        array [2..4] of packed record a:longint; b: byte end _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArray2;
//  pre__FivePackStatArrayPack2{e} _O2_ packed array [2..4] of packed record a:longint; b: byte end _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArrayPack2;
  {$ELSE}
  pre__CharStatArray{e}     := _pre2_CharStatArray;    pre__CharStatArray{e}[1]  := CHR1;    pre__CharStatArray{e}[3] := CHR1;          //@@ _pre3_CharStatArray;   // }}}
  pre__WCharStatArray{e}    := _pre2_WCharStatArray;   pre__WCharStatArray{e}[1] := CHR1;   pre__WCharStatArray{e}[3] := CHR1;          //@@ _pre3_WCharStatArray;  // }}}
  pre__IntStatArray{e}      := _pre2_IntStatArray;     pre__IntStatArray{e}[2]   := 300+ADD;                                            //@@ _pre3_IntStatArray;    // }}}
  pre__AnsiStatArray{e}     := _pre2_AnsiStatArray;    pre__AnsiStatArray{e}[1]  := CHR1;    pre__AnsiStatArray{e}[3] := CHR1+'ab';     //@@ _pre3_AnsiStatArray;   // }}}
  pre__ShortStrStatArray{e} := _pre2_ShortStrStatArray;pre__ShortStrStatArray{e}[1] := CHR1;pre__ShortStrStatArray{e}[3] := CHR1+'ab';  //@@ _pre3_ShortStrStatArray; // }}}

//  pre__FiveStatArray2{e}         := _pre2_FiveStatArray2;         pre__FiveStatArray2{e}        [3].a := -8-ADD;          //@@ _pre3_FiveStatArray2;
//  pre__FiveStatArrayPack2{e}     := _pre2_FiveStatArrayPack2;     pre__FiveStatArrayPack2{e}    [3].a := -8-ADD;          //@@ _pre3_FiveStatArrayPack2;
//  pre__FivePackStatArray2{e}     := _pre2_FivePackStatArray2;     pre__FivePackStatArray2{e}    [3].a := -8-ADD;          //@@ _pre3_FivePackStatArray2;
//  pre__FivePackStatArrayPack2{e} := _pre2_FivePackStatArrayPack2; pre__FivePackStatArrayPack2{e}[3].a := -8-ADD;          //@@ _pre3_FivePackStatArrayPack2;
  {$ENDIF}
  {$ENDIF} // TestPointer
{$ENDIF}
{$ENDIF}

  {$IFnDEF TestAssign}
  pre__CharStatArray2{e}     _O2_ TCharStatArray         _EQ_      (CHR1, 'c',CHR1, 'B','c');          //@@ _pre3_CharStatArray2;
  pre__WCharStatArray2{e}    _O2_ TWCharStatArray        _EQ_      (CHR1, 'c',CHR1, 'B','d');          //@@ _pre3_WCharStatArray2;
  pre__IntStatArray2{e}      _O2_ TIntStatArray          _EQ_      (-2, 200+ADD, 2, 0, 1);          //@@ _pre3_IntStatArray2;
  pre__AnsiStatArray2{e}     _O2_ TAnsiStatArray         _EQ_      (CHR1, 'c123',CHR1+'ad', 'D','cxx'#9);          //@@ _pre3_AnsiStatArray2;
  pre__ShortStrStatArray2{e} _O2_ TShortStrStatArray     _EQ_      (CHR1, 'c123',CHR1+'ad', 'C','cxx'#9);          //@@ _pre3_ShortStrStatArray2;

  pre__FiveStatArray{e}             _O2_ TFiveStatArray            _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArray;          // }}}
  pre__FiveStatArrayPack{e}         _O2_ TFiveStatArrayPack        _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArrayPack;      // }}}
  pre__FivePackStatArray{e}         _O2_ TFivePackStatArray        _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArray;      // }}}
  pre__FivePackStatArrayPack{e}     _O2_ TFivePackStatArrayPack    _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArrayPack;  // }}}
  pre__RecFiveStatArray{e}          _O2_ TRecFiveStatArray         _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFiveStatArray;       // }}}
  pre__RecFiveStatPackArray{e}      _O2_ TRecFiveStatPackArray     _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFiveStatPackArray;   // }}}
  pre__RecFivePackStatArray{e}      _O2_ TRecFivePackStatArray     _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFivePackStatArray;   // }}}
  pre__RecFivePackStatPackArray{e}  _O2_ TRecFivePackStatPackArray _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFivePackStatPackArray;  // }}}
  {$ELSE}
  pre__CharStatArray2{e}     := _pre2_CharStatArray2;    pre__CharStatArray2{e}[1]  := CHR1;    pre__CharStatArray2{e}[3] := CHR1;          //@@ _pre3_CharStatArray2;  // }}}
  pre__WCharStatArray2{e}    := _pre2_WCharStatArray2;   pre__WCharStatArray2{e}[1] := CHR1;   pre__WCharStatArray2{e}[3] := CHR1;          //@@ _pre3_WCharStatArray2;  // }}}
  pre__IntStatArray2{e}      := _pre2_IntStatArray2;     pre__IntStatArray2{e}[2]   := 200+ADD;                                             //@@ _pre3_IntStatArray2;    // }}}
  pre__AnsiStatArray2{e}     := _pre2_AnsiStatArray2;    pre__AnsiStatArray2{e}[1]  := CHR1;    pre__AnsiStatArray2{e}[3] := CHR1+'ad';     //@@ _pre3_AnsiStatArray2;   // }}}
  pre__ShortStrStatArray2{e} := _pre2_ShortStrStatArray2;pre__ShortStrStatArray2{e}[1] := CHR1;pre__ShortStrStatArray2{e}[3] := CHR1+'ad';  //@@ _pre3_ShortStrStatArray2;  // }}}

  pre__FiveStatArray{e}            := _pre2_FiveStatArray;            pre__FiveStatArray{e}           [3].a := -8-ADD;          //@@ _pre3_FiveStatArray;  // }}}
  pre__FiveStatArrayPack{e}        := _pre2_FiveStatArrayPack;        pre__FiveStatArrayPack{e}       [3].a := -8-ADD;          //@@ _pre3_FiveStatArrayPack;  // }}}
  pre__FivePackStatArray{e}        := _pre2_FivePackStatArray;        pre__FivePackStatArray{e}       [3].a := -8-ADD;          //@@ _pre3_FivePackStatArray;  // }}}
  pre__FivePackStatArrayPack{e}    := _pre2_FivePackStatArrayPack;    pre__FivePackStatArrayPack{e}   [3].a := -8-ADD;          //@@ _pre3_FivePackStatArrayPack;  // }}}
  pre__RecFiveStatArray{e}         := _pre2_RecFiveStatArray;         pre__RecFiveStatArray{e}        [3].a := -8-ADD;          //@@ _pre3_RecFiveStatArray;  // }}}
  pre__RecFiveStatPackArray{e}     := _pre2_RecFiveStatPackArray;     pre__RecFiveStatPackArray{e}    [3].a := -8-ADD;          //@@ _pre3_RecFiveStatPackArray;  // }}}
  pre__RecFivePackStatArray{e}     := _pre2_RecFivePackStatArray;     pre__RecFivePackStatArray{e}    [3].a := -8-ADD;          //@@ _pre3_RecFivePackStatArray;  // }}}
  pre__RecFivePackStatPackArray{e} := _pre2_RecFivePackStatPackArray; pre__RecFivePackStatPackArray{e}[3].a := -8-ADD;          //@@ _pre3_RecFivePackStatPackArray;  // }}}
  {$ENDIF}

  pre__Enum{e}  _OP_ TEnum(EnVal3);          //@@ _pre3_Enum;
  pre__Enum1{e} _OP_ TEnumSub(EnVal2);         //@@ _pre3_Enum1;   // subset;
  pre__Enum2{e} _OP_ TEnum2(EnVal21);          //@@ _pre3_Enum2;
  pre__Enum3{e} _OP_ TEnum2(EnVal25);          //@@ _pre3_Enum3;

  pre__Set{e} _OP_ TSet([EnVal2, EnVal4]);          //@@ _pre3_Set;


  {$IFnDEF TestAssign}
  pre__FiveRec{e}             _O2_ TRecordFive      _EQ_ (a:-22-ADD;b:44);          //@@ _pre3_FiveRec;
  {$ELSE}
  pre__FiveRec{e}   := _pre2_FiveRec;       pre__FiveRec{e}.a := -22-ADD;          //@@ _pre3_FiveRec;  // }}
  {$ENDIF}


// array dyn/stat / nested of record/class/char/num ... bitpacked
// record
// object
// class
// class of
// type

// pointer / deref for all above

// self in instance / class

  pre__IntfUnknown{e} _OP_ IUnknown ( nil );          //@@ _pre3_IntfUnknown;

{$UNDEF _BLOCK_}
{$UNDEF _BLOCK2_}

