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

  pre__Bool1{e}         _OP_ Boolean     (False);          //@@ _pre3_Bool1;
  pre__Bool2{e}         _OP_ Boolean     (True);           //@@ _pre3_Bool2;

  pre__Real{e}          _OP_ Real        (50.25                + ADD);          //@@ _pre3_Real;
  pre__Single{e}        _OP_ Single      (100.125              + ADD);          //@@ _pre3_Single;
  pre__Double{e}        _OP_ Double      (1000.125             + ADD);          //@@ _pre3_Double;
  pre__Extended{e}      _OP_ Extended    (10000.175            + ADD);          //@@ _pre3_Extended;
  //pre__Comp{e}        _OP_ Comp        (50.125               + ADD);          //@@ //_pre3_Comp;
  {$IFDEF TestAssign}
  {$IFDEF TestType}
//  pre__Currency{e}      _OP_ TxCurrency    (125.123              + ADD);          //@@ _pre3_Currency;
  {$ELSE}
  pre__Currency{e}      _OP_ Currency    (125.123              + ADD);          //@@ _pre3_Currency;
  {$ENDIF}
  {$ELSE}
  pre__Currency{e}    _OP_ Currency    (-125.123             + ADD);          //@@ _pre3_Currency;
  {$ENDIF}

  pre__Real_2{e}        _OP_ Real        (-50.25               + ADD);          //@@ _pre3_Real_2;
  pre__Single_2{e}      _OP_ Single      (-100.125             + ADD);          //@@ _pre3_Single_2;
  pre__Double_2{e}      _OP_ Double      (-1000.125            + ADD);          //@@ _pre3_Double_2;
  pre__Extended_2{e}    _OP_ Extended    (-10000.175           + ADD);          //@@ _pre3_Extended_2;
  //pre__Comp_2{e}      _OP_ Comp        (-150.125             + ADD);          //@@ //_pre3_Comp_2;
  {$IFDEF TestAssign}
  {$IFDEF TestType}
//  pre__Currency_2{e}    _OP_ TxCurrency_2    (-125.123             + ADD);          //@@ _pre3_Currency_2;
  {$ELSE}
  pre__Currency_2{e}    _OP_ Currency    (-125.123             + ADD);          //@@ _pre3_Currency_2;
  {$ENDIF}
  {$ELSE}
  pre__Currency_2{e}    _OP_ Currency    (-125.123             + ADD);          //@@ _pre3_Currency_2;
  {$ENDIF}

  pre__Ptr1{e}          _OP_ Pointer     (0                         );          //@@ _pre3_Ptr1;
  pre__Ptr2{e}          _OP_ Pointer     (1000                 + ADD);          //@@ _pre3_Ptr2;

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

  pre__Ansi5_Int{e} _O2_ PtrUInt _EQ_ (0);    //@@ _pre3_Ansi5_Int;
  {$IFDEF TestAssign}
  pre__Ansi5_Int{e} _OP_ PtrUInt(pre__Ansi5{e});    //@@ //}}
  {$ENDIF}

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
  {$IFDEF TestType} // incomplete values
  pre__ShortRec{e}       := pre__ShortRec;   pre__ShortRec{e}.st[1] := CHR1; pre__ShortRec{e}.st[3] := CHR1;      //@@ _pre3_ShortRec; // }}
  {$ELSE}
  pre__ShortRec{e}       := _pre2_ShortRec;   pre__ShortRec{e}.st[1] := CHR1; pre__ShortRec{e}.st[3] := CHR1;      //@@ _pre3_ShortRec; // }}
  {$ENDIF}
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

  pre__IntDynArray4_Int{e} _O2_ PtrUInt _EQ_ (0);    //@@ _pre3_IntDynArray4_Int;

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

  pre__IntDynArray4_Int{e} _OP_ PtrUInt(pre__IntDynArray4{e});    //@@ //}}

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

  pre__ArrayEnum1{e}      _O2_  array [TEnum]          of word  _EQ_ (500+ADD,701,702,703); //@@ _pre3_ArrayEnum1;
  pre__ArrayEnumSub1{e}   _O2_  array [TEnumSub]       of word  _EQ_ (600+ADD,801);         //@@ _pre3_ArrayEnumSub1;
  pre__ArrayEnum2{e}      _O2_  array [EnVal1..EnVal4] of word  _EQ_ (300+ADD,701,702,703); //@@ _pre3_ArrayEnum2;
  pre__ArrayEnumSub2{e}   _O2_  array [EnVal1..EnVal2] of word  _EQ_ (400+ADD,801);         //@@ _pre3_ArrayEnumSub2;
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

  pre__ArrayEnum1{e}    := _pre2_ArrayEnum1;    pre__ArrayEnum1{e}[EnVal1]    := 500+ADD;   //@@ _pre3_ArrayEnum1; // }}
  pre__ArrayEnumSub1{e} := _pre2_ArrayEnumSub1; pre__ArrayEnumSub1{e}[EnVal1] := 600+ADD;   //@@ _pre3_ArrayEnumSub1; // }}
  pre__ArrayEnum2{e}    := _pre2_ArrayEnum2;    pre__ArrayEnum2{e}[EnVal1]    := 300+ADD;   //@@ _pre3_ArrayEnum2; // }}
  pre__ArrayEnumSub2{e} := _pre2_ArrayEnumSub2; pre__ArrayEnumSub2{e}[EnVal1] := 400+ADD;   //@@ _pre3_ArrayEnumSub2; // }}
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

  pre__ArrayEnum3{e}      _O2_  TArrayEnum         _EQ_ (200+ADD,701,702,703); //@@ _pre3_ArrayEnum3;
  pre__ArrayEnumSub3{e}   _O2_  TArrayEnumSub      _EQ_ (100+ADD,801);         //@@ _pre3_ArrayEnumSub3;
  pre__ArrayEnum4{e}      _O2_  TArrayEnumElem     _EQ_ (800+ADD,701,702,703); //@@ _pre3_ArrayEnum4;
  pre__ArrayEnumSub4{e}   _O2_  TArrayEnumSubElem  _EQ_ (700+ADD,801);         //@@ _pre3_ArrayEnumSub4;
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

  pre__ArrayEnum3{e}    := _pre2_ArrayEnum3;    pre__ArrayEnum3{e}[EnVal1]    := 200+ADD;   //@@ _pre3_ArrayEnum3; // }}
  pre__ArrayEnumSub3{e} := _pre2_ArrayEnumSub3; pre__ArrayEnumSub3{e}[EnVal1] := 100+ADD;   //@@ _pre3_ArrayEnumSub3; // }}
  pre__ArrayEnum4{e}    := _pre2_ArrayEnum4;    pre__ArrayEnum4{e}[EnVal1]    := 800+ADD;   //@@ _pre3_ArrayEnum4; // }}
  pre__ArrayEnumSub4{e} := _pre2_ArrayEnumSub4; pre__ArrayEnumSub4{e}[EnVal1] := 700+ADD;   //@@ _pre3_ArrayEnumSub4; // }}
  {$ENDIF}

  pre__Enum{e}  _OP_ TEnum(EnVal3);          //@@ _pre3_Enum;
  pre__EnumA{e} _OP_ TEnum(EnVal1);          //@@ _pre3_EnumA;
  pre__Enum1{e} _OP_ TEnumSub(EnVal2);         //@@ _pre3_Enum1;   // subset;
  pre__Enum2{e} _OP_ TEnum2(EnVal21);          //@@ _pre3_Enum2;
  pre__Enum3{e} _OP_ TEnum2(EnVal25);          //@@ _pre3_Enum3;

  pre__Set{e}      _OP_ TSet([EnVal2, EnVal4]);          //@@ _pre3_Set;
  pre__SmallSet{e} _OP_ TSmallRangeSet([22, 24,25]);     //@@ _pre3_SmallSet;


  {$IFnDEF TestAssign}
  pre__BitPackBoolArray{e}      _O2_ TBitPackBoolArray    _EQ_ (True, False, True, True);          //@@ _pre3_BitPackBoolArray; // }}
  pre__BitPackTinyArray{e}      _O2_ TBitPackTinyArray    _EQ_ (1, 0, 3, 2);          //@@ _pre3_BitPackTinyArray; // }}
  pre__BitPackTinyNegArray{e}   _O2_ TBitPackTinyNegArray _EQ_ (2, 6{-2}, 0, 7{-1});          //@@ _pre3_BitPackTinyNegArray; // }}
  pre__BitPackEnumArray{e}      _O2_ TBitPackEnumArray    _EQ_ (EnVal3, EnVal1, EnVal2, EnVal3);          //@@ _pre3_BitPackEnumArray; // }}
  pre__BitPackEnum3Array{e}     _O2_ TBitPackEnum3Array   _EQ_ (EnVal32, EnVal32, EnVal31, EnVal32);          //@@ _pre3_BitPackEnum3Array; // }}
  pre__BitPackSetArray{e}       _O2_ TBitPackSetArray     _EQ_ ([EnVal3, EnVal1], [], [EnVal3], [EnVal1]);          //@@ _pre3_BitPackSetArray; // }}
  pre__BitPackSet3Array{e}      _O2_ TBitPackSet3Array    _EQ_ ([EnVal31, EnVal32], [], [EnVal31], [EnVal32]);          //@@ _pre3_BitPackSet3Array; // }}

  pre__BitPackBoolArray2{e}      _O2_ TBitPackBoolArray2    _EQ_ ((True, False, True), (False, True, True));          //@@ _pre3_BitPackBoolArray2; // }}
  pre__BitPackTinyArray2{e}      _O2_ TBitPackTinyArray2    _EQ_ ((1, 0, 3), (2,3,0));          //@@ _pre3_BitPackTinyArray2; // }}
  pre__BitPackTinyNegArray2{e}   _O2_ TBitPackTinyNegArray2 _EQ_ ((2, 6{-2}, 0), (1, 0, 7{-1}));          //@@ _pre3_BitPackTinyNegArray2; // }}
  pre__BitPackEnumArray2{e}      _O2_ TBitPackEnumArray2    _EQ_ ((EnVal3, EnVal1, EnVal2), (EnVal1, EnVal4, EnVal2));          //@@ _pre3_BitPackEnumArray2; // }}
  pre__BitPackEnum3Array2{e}     _O2_ TBitPackEnum3Array2   _EQ_ ((EnVal32, EnVal32, EnVal31), (EnVal31, EnVal31, EnVal32));          //@@ _pre3_BitPackEnum3Array2; // }}
//  pre__BitPackSetArray2{e}       _O2_ TBitPackSetArray2     _EQ_ (([EnVal3, EnVal1], [], [EnVal3]), ([],[EnVal1,EnVal2],[EnVal1]));          //@@ _pre3_BitPackSetArray2; // }}
//  pre__BitPackSet3Array2{e}      _O2_ TBitPackSet3Array2    _EQ_ (([EnVal31, EnVal32], [], [EnVal31]), ([EnVal31,EnVal32],[],[EnVal32]));          //@@ _pre3_BitPackSet3Array2; // }}

  pre__BitPackBoolRecord{e}      _O2_ TBitPackBoolRecord    _EQ_ (a:True; b:False; c:True; d: True; e: False);          //@@ _pre3_BitPackBoolRecord; // }}
  pre__BitPackTinyRecord{e}      _O2_ TBitPackTinyRecord    _EQ_ (a:1; b:1; c:0; d: 3; e: 0);          //@@ _pre3_BitPackTinyRecord; // }}
  pre__BitPackTinyNegRecord{e}   _O2_ TBitPackTinyNegRecord _EQ_ (a:3; b:6{-2}; c:7{-1}; d: 0; e: 1);          //@@ _pre3_BitPackTinyNegRecord; // }}
  pre__BitPackEnumRecord{e}      _O2_ TBitPackEnumRecord    _EQ_ (a:EnVal3; b:EnVal1; c:EnVal2; d: EnVal2; e: EnVal1);          //@@ _pre3_BitPackEnumRecord; // }}
  pre__BitPackEnum3Record{e}     _O2_ TBitPackEnum3Record   _EQ_ (a:EnVal31; b:EnVal32; c:EnVal31; d: EnVal31; e: EnVal32);          //@@ _pre3_BitPackEnum3Record; // }}
  pre__BitPackSetRecord{e}       _O2_ TBitPackSetRecord     _EQ_ (a:[EnVal3]; b:[]; c:[EnVal1,EnVal2]; d: [EnVal2]; e: [EnVal1,EnVal3]);          //@@ _pre3_BitPackSetRecord; // }}
  pre__BitPackSet3Record{e}      _O2_ TBitPackSet3Record    _EQ_ (a:[EnVal31]; b:[]; c:[EnVal31,EnVal32]; d: [EnVal32]; e: [EnVal31]);          //@@ _pre3_BitPackSet3Record; // }}

  pre__BitPackBoolArrayRecord{e}      _O2_ TBitPackBoolArrayRecord    _EQ_ (a:(True, False, True, True); b:(False, True, True, False));          //@@ _pre3_BitPackBoolArrayRecord; // }}
  pre__BitPackTinyArrayRecord{e}      _O2_ TBitPackTinyArrayRecord    _EQ_ (a:(1, 0, 3, 2); b:(2,3,0,1));          //@@ _pre3_BitPackTinyArrayRecord; // }}
  pre__BitPackTinyNegArrayRecord{e}   _O2_ TBitPackTinyNegArrayRecord _EQ_ (a:(2, 6{-2}, 0, 7{-1}); b:(1, 0, 7{-1},2));          //@@ _pre3_BitPackTinyNegArrayRecord; // }}
  pre__BitPackEnumArrayRecord{e}      _O2_ TBitPackEnumArrayRecord    _EQ_ (a:(EnVal3, EnVal1, EnVal2, EnVal4); b:(EnVal1, EnVal4, EnVal2, EnVal4));          //@@ _pre3_BitPackEnumArrayRecord; // }}
  pre__BitPackEnum3ArrayRecord{e}     _O2_ TBitPackEnum3ArrayRecord   _EQ_ (a:(EnVal32, EnVal32, EnVal31, EnVal31); b:(EnVal31, EnVal31, EnVal32, EnVal31));          //@@ _pre3_BitPackEnum3ArrayRecord; // }}
  pre__BitPackSetArrayRecord{e}       _O2_ TBitPackSetArrayRecord     _EQ_ (a:([EnVal3, EnVal1], [], [EnVal3], []); b:([],[EnVal1,EnVal2],[EnVal1],[EnVal4]));          //@@ _pre3_BitPackSetArrayRecord; // }}
  pre__BitPackSet3ArrayRecord{e}      _O2_ TBitPackSet3ArrayRecord    _EQ_ (a:([EnVal31, EnVal32], [], [EnVal31],[EnVal31,EnVal32]); b:([EnVal31,EnVal32],[],[EnVal32],[]));          //@@ _pre3_BitPackSet3ArrayRecord; // }}


  pre__FpDbgValueSize{e}   _O2_ TFpDbgValueSize    _EQ_ (Size: 0; BitSize: 2);          //@@ _pre3_FpDbgValueSize; // }}

  {$ELSE}
  pre__BitPackBoolArray{e}    := _pre2_BitPackBoolArray;         //@@ _pre3_BitPackBoolArray; // }}
  pre__BitPackTinyArray{e}    := _pre2_BitPackTinyArray;         //@@ _pre3_BitPackTinyArray; // }}
  pre__BitPackTinyNegArray{e} := _pre2_BitPackTinyNegArray;      //@@ _pre3_BitPackTinyNegArray; // }}
  pre__BitPackEnumArray{e}    := _pre2_BitPackEnumArray;   //@@ _pre3_BitPackEnumArray; // }}
  pre__BitPackEnum3Array{e}   := _pre2_BitPackEnum3Array;  //@@ _pre3_BitPackEnum3Array; // }}
  pre__BitPackSetArray{e}     := _pre2_BitPackSetArray;    //@@ _pre3_BitPackSetArray; // }}
  pre__BitPackSet3Array{e}    := _pre2_BitPackSet3Array;   //@@ _pre3_BitPackSet3Array; // }}

  pre__BitPackBoolArray2{e}    := _pre2_BitPackBoolArray2;         //@@ _pre3_BitPackBoolArray2; // }}
  pre__BitPackTinyArray2{e}    := _pre2_BitPackTinyArray2;         //@@ _pre3_BitPackTinyArray2; // }}
  pre__BitPackTinyNegArray2{e} := _pre2_BitPackTinyNegArray2;      //@@ _pre3_BitPackTinyNegArray2; // }}
  pre__BitPackEnumArray2{e}    := _pre2_BitPackEnumArray2;   //@@ _pre3_BitPackEnumArray2; // }}
  pre__BitPackEnum3Array2{e}   := _pre2_BitPackEnum3Array2;  //@@ _pre3_BitPackEnum3Array2; // }}
//  pre__BitPackSetArray2{e}     := _pre2_BitPackSetArray2;    //@@ _pre3_BitPackSetArray2; // }}
//  pre__BitPackSet3Array2{e}    := _pre2_BitPackSet3Array2;   //@@ _pre3_BitPackSet3Array2; // }}

  {$IFnDEF TestType}
  pre__BitPackBoolRecord{e}    := _pre2_BitPackBoolRecord;      //@@ _pre3_BitPackBoolRecord; // }}
  pre__BitPackTinyRecord{e}    := _pre2_BitPackTinyRecord;      //@@ _pre3_BitPackTinyRecord; // }}
  pre__BitPackTinyNegRecord{e} := _pre2_BitPackTinyNegRecord;   //@@ _pre3_BitPackTinyNegRecord; // }}
  pre__BitPackEnumRecord{e}    := _pre2_BitPackEnumRecord;      //@@ _pre3_BitPackEnumRecord; // }}
  pre__BitPackEnum3Record{e}   := _pre2_BitPackEnum3Record;     //@@ _pre3_BitPackEnum3Record; // }}
  pre__BitPackSetRecord{e}     := _pre2_BitPackSetRecord;       //@@ _pre3_BitPackSetRecord; // }}
  pre__BitPackSet3Record{e}    := _pre2_BitPackSet3Record;      //@@ _pre3_BitPackSet3Record; // }}

  pre__BitPackBoolArrayRecord{e}    := _pre2_BitPackBoolArrayRecord;         //@@ _pre3_BitPackBoolArrayRecord; // }}
  pre__BitPackTinyArrayRecord{e}    := _pre2_BitPackTinyArrayRecord;         //@@ _pre3_BitPackTinyArrayRecord; // }}
  pre__BitPackTinyNegArrayRecord{e} := _pre2_BitPackTinyNegArrayRecord;      //@@ _pre3_BitPackTinyNegArrayRecord; // }}
  pre__BitPackEnumArrayRecord{e}    := _pre2_BitPackEnumArrayRecord;   //@@ _pre3_BitPackEnumArrayRecord; // }}
  pre__BitPackEnum3ArrayRecord{e}   := _pre2_BitPackEnum3ArrayRecord;  //@@ _pre3_BitPackEnum3ArrayRecord; // }}
  pre__BitPackSetArrayRecord{e}     := _pre2_BitPackSetArrayRecord;    //@@ _pre3_BitPackSetArrayRecord; // }}
  pre__BitPackSet3ArrayRecord{e}    := _pre2_BitPackSet3ArrayRecord;   //@@ _pre3_BitPackSet3ArrayRecord; // }}

  pre__FpDbgValueSize{e}  :=  _pre2_FpDbgValueSize;  pre__FpDbgValueSize{e}.Size  :=  0;  //@@ _pre3_FpDbgValueSize; // }}
  {$ELSE}
  pre__BitPackBoolRecord{e}    := TxBitPackBoolRecord(_pre2_BitPackBoolRecord);      //@@ _pre3_BitPackBoolRecord; // }}
  pre__BitPackTinyRecord{e}    := TxBitPackTinyRecord(_pre2_BitPackTinyRecord);      //@@ _pre3_BitPackTinyRecord; // }}
  pre__BitPackTinyNegRecord{e} := TxBitPackTinyNegRecord(_pre2_BitPackTinyNegRecord);   //@@ _pre3_BitPackTinyNegRecord; // }}
  pre__BitPackEnumRecord{e}    := TxBitPackEnumRecord(_pre2_BitPackEnumRecord);      //@@ _pre3_BitPackEnumRecord; // }}
  pre__BitPackEnum3Record{e}   := TxBitPackEnum3Record(_pre2_BitPackEnum3Record);     //@@ _pre3_BitPackEnum3Record; // }}
  pre__BitPackSetRecord{e}     := TxBitPackSetRecord(_pre2_BitPackSetRecord);       //@@ _pre3_BitPackSetRecord; // }}
  pre__BitPackSet3Record{e}    := TxBitPackSet3Record(_pre2_BitPackSet3Record);      //@@ _pre3_BitPackSet3Record; // }}

  pre__BitPackBoolArrayRecord{e}    := TxBitPackBoolArrayRecord(_pre2_BitPackBoolArrayRecord);         //@@ _pre3_BitPackBoolArrayRecord; // }}
  pre__BitPackTinyArrayRecord{e}    := TxBitPackTinyArrayRecord(_pre2_BitPackTinyArrayRecord);         //@@ _pre3_BitPackTinyArrayRecord; // }}
  pre__BitPackTinyNegArrayRecord{e} := TxBitPackTinyNegArrayRecord(_pre2_BitPackTinyNegArrayRecord);      //@@ _pre3_BitPackTinyNegArrayRecord; // }}
  pre__BitPackEnumArrayRecord{e}    := TxBitPackEnumArrayRecord(_pre2_BitPackEnumArrayRecord);   //@@ _pre3_BitPackEnumArrayRecord; // }}
  pre__BitPackEnum3ArrayRecord{e}   := TxBitPackEnum3ArrayRecord(_pre2_BitPackEnum3ArrayRecord);  //@@ _pre3_BitPackEnum3ArrayRecord; // }}
  pre__BitPackSetArrayRecord{e}     := TxBitPackSetArrayRecord(_pre2_BitPackSetArrayRecord);    //@@ _pre3_BitPackSetArrayRecord; // }}
  pre__BitPackSet3ArrayRecord{e}    := TxBitPackSet3ArrayRecord(_pre2_BitPackSet3ArrayRecord);   //@@ _pre3_BitPackSet3ArrayRecord; // }}

  pre__FpDbgValueSize{e}  :=  TxFpDbgValueSize(_pre2_FpDbgValueSize);   //@@ _pre3_FpDbgValueSize; // }}
  {$ENDIF}

  {$ENDIF}


{$IFnDEF TestParam}
{$IFnDEF TestArg}
{$IFnDEF TestPointer}
  pre__Set2{e}      _O2_ set of TEnum       _EQ_ ([EnVal1, EnVal4]);      //@@ _pre3_Set2;
  pre__SmallSet2{e} _O2_ set of TSmallRange _EQ_ ([21, 24,25]);           //@@ _pre3_SmallSet2;
{$ENDIF}
{$ENDIF}
{$ENDIF}

  {$IFnDEF TestAssign}
    pre__FiveRec{e}       _O2_ TRecordFive      _EQ_ (a:-22-ADD;b:44);          //@@ _pre3_FiveRec;
    pre__Rec3S{e}         _O2_ TRecord3Int64    _EQ_ (a:-22;b:44;c:1000+ADD);          //@@ _pre3_Rec3S;
    pre__Rec3U{e}         _O2_ TRecord3QWord    _EQ_ (a:111;b:44;c:1000+ADD);          //@@ _pre3_Rec3U;

    pre__Obj3{e}          _O2_ TObject3Int64          _EQ_ (a:-22;b:44;c:4000+ADD);          //@@ _pre3_Obj3;
    pre__Obj3Ex{e}        _O2_ TObject3Int64Ex        _EQ_ (a:-22;b:44;c:4100+ADD; d: 555);  //@@ _pre3_Obj3Ex;
    {$IFnDEF TestConst}
    {$IFnDEF TestAssignGC}
    {$IFnDEF TestType} // will work in fpc 3.2.0 upwards
    pre__Obj3C{e}         _O2_ TObjectCreate3Int64   ;  //@@ _pre3_Obj3C; // }}}
    pre__Obj3ExC{e}       _O2_ TObjectCreate3Int64Ex ;  //@@ _pre3_Obj3ExC; // }}}
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  {$ELSE} // TestAssign
    {$IFDEF TestType} // incomplete values
    pre__FiveRec{e} := pre__FiveRec;   pre__FiveRec{e}.a := -22-ADD;          //@@ _pre3_FiveRec;  // }}}
    pre__Rec3S{e}   := pre__Rec3S;     pre__Rec3S{e}.c := 1000+ADD;          //@@ _pre3_Rec3S;  // }}}
    pre__Rec3U{e}   := pre__Rec3U;     pre__Rec3U{e}.c := 1000+ADD;          //@@ _pre3_Rec3U;  // }}}

    pre__Obj3{e}    := pre__Obj3;      pre__Obj3{e}.c    := 4000+ADD;  //@@ _pre3_Obj3;  // }}}
    pre__Obj3Ex{e}  := pre__Obj3Ex;    pre__Obj3Ex{e}.c  := 4100+ADD;  //@@ _pre3_Obj3Ex;  // }}}
    {$ELSE}
    pre__FiveRec{e} := _pre2_FiveRec;   pre__FiveRec{e}.a := -22-ADD;          //@@ _pre3_FiveRec;  // }}}
    pre__Rec3S{e}   := _pre2_Rec3S;     pre__Rec3S{e}.c := 1000+ADD;          //@@ _pre3_Rec3S;  // }}}
    pre__Rec3U{e}   := _pre2_Rec3U;     pre__Rec3U{e}.c := 1000+ADD;          //@@ _pre3_Rec3U;  // }}}

    pre__Obj3{e}    := _pre2_Obj3;      pre__Obj3{e}.c    := 4000+ADD;  //@@ _pre3_Obj3;  // }}}
    pre__Obj3Ex{e}  := _pre2_Obj3Ex;    pre__Obj3Ex{e}.c  := 4100+ADD;  //@@ _pre3_Obj3Ex;  // }}}
    {$ENDIF}

    {$IFnDEF TestConst}
    {$IFnDEF TestType} // will work in fpc 3.2.0 upwards
    pre__Obj3C{e}.Create;   pre__Obj3C{e}.a   := 22; pre__Obj3C{e}.b   := 44; pre__Obj3C{e}.c   := 4200+ADD;  //@@ _pre3_Obj3C;  // }}}}}
    pre__Obj3ExC{e}.Create; pre__Obj3ExC{e}.a := 22; pre__Obj3ExC{e}.b := 44; pre__Obj3ExC{e}.c := 4300+ADD; pre__Obj3ExC{e}.d := 655; //@@ _pre3_Obj3ExC;  // }}}}}
    {$ENDIF}
    {$ENDIF}
  {$ENDIF}


  pre__Instance0{e} _O2_ TClass1 _EQ_ (nil);  //@@ _pre3_Instance0;
  pre__Instance1{e} _O2_ TClass1 _EQ_ (nil);  //@@ _pre3_Instance1;
  pre__Instance1_Int{e} _O2_ PtrUInt _EQ_ (0);    //@@ _pre3_Instance1_Int;
  {$IFDEF TestAssign}
  {$IFDEF TestType}
  pre__Instance1{e} := TxInstance1.Create;    //@@
  {$ELSE}
  pre__Instance1{e} := TClass1.Create;    //@@
  {$ENDIF}
  pre__Instance1{e}.FInt _OP_ 22+ADD;     //@@
  pre__Instance1{e}.FAnsi _OP_ CHR1+'T';  //@@
  pre__Instance1_Int{e} _OP_ PtrUInt(pre__Instance1{e});    //@@ //}}
  {$ENDIF}

  {$IFnDEF TestType}
  {$IFnDEF TestArg}{$IFnDEF TestParam}
  pre__Instance2{e} _O2_ TClass1 _EQ_ (nil);  //@@ _pre3_Instance2;
  pre__Instance2b{e} _O2_ TClass1 _EQ_ (nil);  //@@ _pre3_Instance2b;
  {$IFDEF TestAssign}
  pre__Instance2{e} := TClass1.Create;    //@@
  pre__Instance2b{e} := pre__Instance2{e};    //@@  // }}
  {$ENDIF}
  {$ENDIF}{$ENDIF}
  {$ENDIF}

  {$IFnDEF TestType}
  {$IFnDEF TestAssign}
  pre__Class1Rec{e}     _O2_ TRecordClass1    _EQ_ (Foo: nil);    //@@ _pre3_Class1Rec;
  {$ELSE}
  pre__Class1Rec{e}.Foo  := pre__Instance1{e};  //@@ _pre3_Class1Rec;  // }}}
  {$ENDIF}
  {$ENDIF}


  pre__SomeFunc1Ref{e}    _O2_ TFunc1                 _EQ_      (nil); //@@ _pre3_SomeFunc1Ref;
  pre__SomeProc1Ref{e}    _O2_ TProc1                 _EQ_      (nil); //@@ _pre3_SomeProc1Ref;
  pre__SomeMeth1Ref{e}    _O2_ TMeth1                 _EQ_      (nil); //@@ _pre3_SomeMeth1Ref;
  {$IFDEF TestAssign}
  pre__SomeFunc1Ref{e}    := @SomeFunc1; //@@
  pre__SomeProc1Ref{e}    := @SomeProc1; //@@
  pre__SomeMeth1Ref{e}    := @MyClass2.SomeMeth1; //@@
  //pre__SomeMeth1Ref{e}    := @TMyBaseClass(nil).SomeMeth1; //@@
  {$ENDIF}

// short bool ....
// interface / object vs class / record
// array dyn/stat / nested of record/class/char/num ... bitpacked
// array [ enum ]
// type = array [ enum ]
// class of
// class/record helper
// type
//   b = 1..5;  c = set of b; // set of subrange


// pointer / deref for all above

// self in instance / class

  pre__IntfUnknown{e} _OP_ IUnknown ( nil );          //@@ _pre3_IntfUnknown;
  {$IFnDEF TestType}
  pre__IntfUnknown1{e} _OP_ IUnknown ( nil );          //@@ _pre3_IntfUnknown1;
  {$IFDEF TestAssign}
  pre__IntfUnknown1{e}    := InterfacedObject as IUnknown; //@@
  {$ENDIF}
  {$IFnDEF TestArg}{$IFnDEF TestParam}  // reduce count of param...
  pre__IntfUnknown2{e} _OP_ IUnknown ( nil );          //@@ _pre3_IntfUnknown2;
  pre__IntfUnknown2b{e} _OP_ IUnknown ( nil );          //@@ _pre3_IntfUnknown2b;
  {$IFDEF TestAssign}
  pre__IntfUnknown2{e}    := InterfacedObject2 as IUnknown; //@@
  pre__IntfUnknown2b{e}   := InterfacedObject2 as IUnknown; //@@
  {$ENDIF}
  {$ENDIF}{$ENDIF}
  {$ENDIF}

{$UNDEF _BLOCK_}
{$UNDEF _BLOCK2_}

