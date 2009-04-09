{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Theo Lustenberger

  Abstract:
    Container for derived data from the Unicode data files.

 *****************************************************************************
 *  This file contains derived data from a modified version of the
 *  Unicode data files.
 *
 *  The original data files are available at
 *  http://www.unicode.org/Public/UNIDATA/
 *
 *
 *  COPYRIGHT AND PERMISSION NOTICE
 *
 *  Copyright (c) 1991-2007 Unicode, Inc. All rights reserved. Distributed
 *  under the Terms of Use in http://www.unicode.org/copyright.html.
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a
 *  copy of the Unicode data files and any associated documentation (the "Data
 *  Files") or Unicode software and any associated documentation (the
 *  "Software") to deal in the Data Files or Software without restriction,
 *  including without limitation the rights to use, copy, modify, merge,
 *  publish, distribute, and/or sell copies of the Data Files or Software, and
 *  to permit persons to whom the Data Files or Software are furnished to do
 *  so, provided that (a) the above copyright notice(s) and this permission
 *  notice appear with all copies of the Data Files or Software, (b) both the
 *  above copyright notice(s) and this permission notice appear in associated
 *  documentation, and (c) there is clear notice in each modified Data File or
 *  in the Software as well as in the documentation associated with the Data
 *  File(s) or Software that the data or software has been modified.
 *
 *  THE DATA FILES AND SOFTWARE ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
 *  KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT OF
 *  THIRD PARTY RIGHTS. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR HOLDERS
 *  INCLUDED IN THIS NOTICE BE LIABLE FOR ANY CLAIM, OR ANY SPECIAL INDIRECT OR
 *  CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 *  USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 *  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 *  PERFORMANCE OF THE DATA FILES OR SOFTWARE.
 *
 *  Except as contained in this notice, the name of a copyright holder shall
 *  not be used in advertising or otherwise to promote the sale, use or other
 *  dealings in these Data Files or Software without prior written
 *  authorization of the copyright holder. 
 *****************************************************************************

}

unit LCLUnicodeData;

{$mode objfpc}{$H+}

interface

type TUnicodeBlock = record
    S: longint;
    E: longint;
    PG: string[50];
  end;

const
  MaxUnicodeBlocks = 151; //Warning this is for charactermapdlg.pas. Full Range is 153. See last commented items 
  UnicodeBlocks: array[0..MaxUnicodeBlocks] of TUnicodeBlock = (
    (S: $0020; E: $007F; PG: 'Basic Latin'),  //Warning this is for charactermapdlg.pas: Full range starts at $0000
    (S: $00A0; E: $00FF; PG: 'Latin-1 Supplement'), //Warning this is for charactermapdlg.pas: Full range starts at $0080
    (S: $0100; E: $017F; PG: 'Latin Extended-A'),
    (S: $0180; E: $024F; PG: 'Latin Extended-B'),
    (S: $0250; E: $02AF; PG: 'IPA Extensions'),
    (S: $02B0; E: $02FF; PG: 'Spacing Modifier Letters'),
    (S: $0300; E: $036F; PG: 'Combining Diacritical Marks'),
    (S: $0370; E: $03FF; PG: 'Greek and Coptic'),
    (S: $0400; E: $04FF; PG: 'Cyrillic'),
    (S: $0500; E: $052F; PG: 'Cyrillic Supplement'),
    (S: $0530; E: $058F; PG: 'Armenian'),
    (S: $0590; E: $05FF; PG: 'Hebrew'),
    (S: $0600; E: $06FF; PG: 'Arabic'),
    (S: $0700; E: $074F; PG: 'Syriac'),
    (S: $0750; E: $077F; PG: 'Arabic Supplement'),
    (S: $0780; E: $07BF; PG: 'Thaana'),
    (S: $07C0; E: $07FF; PG: 'NKo'),
    (S: $0900; E: $097F; PG: 'Devanagari'),
    (S: $0980; E: $09FF; PG: 'Bengali'),
    (S: $0A00; E: $0A7F; PG: 'Gurmukhi'),
    (S: $0A80; E: $0AFF; PG: 'Gujarati'),
    (S: $0B00; E: $0B7F; PG: 'Oriya'),
    (S: $0B80; E: $0BFF; PG: 'Tamil'),
    (S: $0C00; E: $0C7F; PG: 'Telugu'),
    (S: $0C80; E: $0CFF; PG: 'Kannada'),
    (S: $0D00; E: $0D7F; PG: 'Malayalam'),
    (S: $0D80; E: $0DFF; PG: 'Sinhala'),
    (S: $0E00; E: $0E7F; PG: 'Thai'),
    (S: $0E80; E: $0EFF; PG: 'Lao'),
    (S: $0F00; E: $0FFF; PG: 'Tibetan'),
    (S: $1000; E: $109F; PG: 'Myanmar'),
    (S: $10A0; E: $10FF; PG: 'Georgian'),
    (S: $1100; E: $11FF; PG: 'Hangul Jamo'),
    (S: $1200; E: $137F; PG: 'Ethiopic'),
    (S: $1380; E: $139F; PG: 'Ethiopic Supplement'),
    (S: $13A0; E: $13FF; PG: 'Cherokee'),
    (S: $1400; E: $167F; PG: 'Unified Canadian Aboriginal Syllabics'),
    (S: $1680; E: $169F; PG: 'Ogham'),
    (S: $16A0; E: $16FF; PG: 'Runic'),
    (S: $1700; E: $171F; PG: 'Tagalog'),
    (S: $1720; E: $173F; PG: 'Hanunoo'),
    (S: $1740; E: $175F; PG: 'Buhid'),
    (S: $1760; E: $177F; PG: 'Tagbanwa'),
    (S: $1780; E: $17FF; PG: 'Khmer'),
    (S: $1800; E: $18AF; PG: 'Mongolian'),
    (S: $1900; E: $194F; PG: 'Limbu'),
    (S: $1950; E: $197F; PG: 'Tai Le'),
    (S: $1980; E: $19DF; PG: 'New Tai Lue'),
    (S: $19E0; E: $19FF; PG: 'Khmer Symbols'),
    (S: $1A00; E: $1A1F; PG: 'Buginese'),
    (S: $1B00; E: $1B7F; PG: 'Balinese'),
    (S: $1D00; E: $1D7F; PG: 'Phonetic Extensions'),
    (S: $1D80; E: $1DBF; PG: 'Phonetic Extensions Supplement'),
    (S: $1DC0; E: $1DFF; PG: 'Combining Diacritical Marks Supplement'),
    (S: $1E00; E: $1EFF; PG: 'Latin Extended Additional'),
    (S: $1F00; E: $1FFF; PG: 'Greek Extended'),
    (S: $2000; E: $206F; PG: 'General Punctuation'),
    (S: $2070; E: $209F; PG: 'Superscripts and Subscripts'),
    (S: $20A0; E: $20CF; PG: 'Currency Symbols'),
    (S: $20D0; E: $20FF; PG: 'Combining Diacritical Marks for Symbols'),
    (S: $2100; E: $214F; PG: 'Letterlike Symbols'),
    (S: $2150; E: $218F; PG: 'Number Forms'),
    (S: $2190; E: $21FF; PG: 'Arrows'),
    (S: $2200; E: $22FF; PG: 'Mathematical Operators'),
    (S: $2300; E: $23FF; PG: 'Miscellaneous Technical'),
    (S: $2400; E: $243F; PG: 'Control Pictures'),
    (S: $2440; E: $245F; PG: 'Optical Character Recognition'),
    (S: $2460; E: $24FF; PG: 'Enclosed Alphanumerics'),
    (S: $2500; E: $257F; PG: 'Box Drawing'),
    (S: $2580; E: $259F; PG: 'Block Elements'),
    (S: $25A0; E: $25FF; PG: 'Geometric Shapes'),
    (S: $2600; E: $26FF; PG: 'Miscellaneous Symbols'),
    (S: $2700; E: $27BF; PG: 'Dingbats'),
    (S: $27C0; E: $27EF; PG: 'Miscellaneous Mathematical Symbols-A'),
    (S: $27F0; E: $27FF; PG: 'Supplemental Arrows-A'),
    (S: $2800; E: $28FF; PG: 'Braille Patterns'),
    (S: $2900; E: $297F; PG: 'Supplemental Arrows-B'),
    (S: $2980; E: $29FF; PG: 'Miscellaneous Mathematical Symbols-B'),
    (S: $2A00; E: $2AFF; PG: 'Supplemental Mathematical Operators'),
    (S: $2B00; E: $2BFF; PG: 'Miscellaneous Symbols and Arrows'),
    (S: $2C00; E: $2C5F; PG: 'Glagolitic'),
    (S: $2C60; E: $2C7F; PG: 'Latin Extended-C'),
    (S: $2C80; E: $2CFF; PG: 'Coptic'),
    (S: $2D00; E: $2D2F; PG: 'Georgian Supplement'),
    (S: $2D30; E: $2D7F; PG: 'Tifinagh'),
    (S: $2D80; E: $2DDF; PG: 'Ethiopic Extended'),
    (S: $2E00; E: $2E7F; PG: 'Supplemental Punctuation'),
    (S: $2E80; E: $2EFF; PG: 'CJK Radicals Supplement'),
    (S: $2F00; E: $2FDF; PG: 'Kangxi Radicals'),
    (S: $2FF0; E: $2FFF; PG: 'Ideographic Description Characters'),
    (S: $3000; E: $303F; PG: 'CJK Symbols and Punctuation'),
    (S: $3040; E: $309F; PG: 'Hiragana'),
    (S: $30A0; E: $30FF; PG: 'Katakana'),
    (S: $3100; E: $312F; PG: 'Bopomofo'),
    (S: $3130; E: $318F; PG: 'Hangul Compatibility Jamo'),
    (S: $3190; E: $319F; PG: 'Kanbun'),
    (S: $31A0; E: $31BF; PG: 'Bopomofo Extended'),
    (S: $31C0; E: $31EF; PG: 'CJK Strokes'),
    (S: $31F0; E: $31FF; PG: 'Katakana Phonetic Extensions'),
    (S: $3200; E: $32FF; PG: 'Enclosed CJK Letters and Months'),
    (S: $3300; E: $33FF; PG: 'CJK Compatibility'),
    (S: $3400; E: $4DBF; PG: 'CJK Unified Ideographs Extension A'),
    (S: $4DC0; E: $4DFF; PG: 'Yijing Hexagram Symbols'),
    (S: $4E00; E: $9FFF; PG: 'CJK Unified Ideographs'),
    (S: $A000; E: $A48F; PG: 'Yi Syllables'),
    (S: $A490; E: $A4CF; PG: 'Yi Radicals'),
    (S: $A700; E: $A71F; PG: 'Modifier Tone Letters'),
    (S: $A720; E: $A7FF; PG: 'Latin Extended-D'),
    (S: $A800; E: $A82F; PG: 'Syloti Nagri'),
    (S: $A840; E: $A87F; PG: 'Phags-pa'),
    (S: $AC00; E: $D7AF; PG: 'Hangul Syllables'),
    (S: $D800; E: $DB7F; PG: 'High Surrogates'),
    (S: $DB80; E: $DBFF; PG: 'High Private Use Surrogates'),
    (S: $DC00; E: $DFFF; PG: 'Low Surrogates'),
    (S: $E000; E: $F8FF; PG: 'Private Use Area'),
    (S: $F900; E: $FAFF; PG: 'CJK Compatibility Ideographs'),
    (S: $FB00; E: $FB4F; PG: 'Alphabetic Presentation Forms'),
    (S: $FB50; E: $FDFF; PG: 'Arabic Presentation Forms-A'),
    (S: $FE00; E: $FE0F; PG: 'Variation Selectors'),
    (S: $FE10; E: $FE1F; PG: 'Vertical Forms'),
    (S: $FE20; E: $FE2F; PG: 'Combining Half Marks'),
    (S: $FE30; E: $FE4F; PG: 'CJK Compatibility Forms'),
    (S: $FE50; E: $FE6F; PG: 'Small Form Variants'),
    (S: $FE70; E: $FEFF; PG: 'Arabic Presentation Forms-B'),
    (S: $FF00; E: $FFEF; PG: 'Halfwidth and Fullwidth Forms'),
    (S: $FFF0; E: $FFFF; PG: 'Specials'),
    (S: $10000; E: $1007F; PG: 'Linear B Syllabary'),
    (S: $10080; E: $100FF; PG: 'Linear B Ideograms'),
    (S: $10100; E: $1013F; PG: 'Aegean Numbers'),
    (S: $10140; E: $1018F; PG: 'Ancient Greek Numbers'),
    (S: $10300; E: $1032F; PG: 'Old Italic'),
    (S: $10330; E: $1034F; PG: 'Gothic'),
    (S: $10380; E: $1039F; PG: 'Ugaritic'),
    (S: $103A0; E: $103DF; PG: 'Old Persian'),
    (S: $10400; E: $1044F; PG: 'Deseret'),
    (S: $10450; E: $1047F; PG: 'Shavian'),
    (S: $10480; E: $104AF; PG: 'Osmanya'),
    (S: $10800; E: $1083F; PG: 'Cypriot Syllabary'),
    (S: $10900; E: $1091F; PG: 'Phoenician'),
    (S: $10A00; E: $10A5F; PG: 'Kharoshthi'),
    (S: $12000; E: $123FF; PG: 'Cuneiform'),
    (S: $12400; E: $1247F; PG: 'Cuneiform Numbers and Punctuation'),
    (S: $1D000; E: $1D0FF; PG: 'Byzantine Musical Symbols'),
    (S: $1D100; E: $1D1FF; PG: 'Musical Symbols'),
    (S: $1D200; E: $1D24F; PG: 'Ancient Greek Musical Notation'),
    (S: $1D300; E: $1D35F; PG: 'Tai Xuan Jing Symbols'),
    (S: $1D360; E: $1D37F; PG: 'Counting Rod Numerals'),
    (S: $1D400; E: $1D7FF; PG: 'Mathematical Alphanumeric Symbols'),
    (S: $20000; E: $2A6DF; PG: 'CJK Unified Ideographs Extension B'),
    (S: $2F800; E: $2FA1F; PG: 'CJK Compatibility Ideographs Supplement'),
    (S: $E0000; E: $E007F; PG: 'Tags'),
    (S: $E0100; E: $E01EF; PG: 'Variation Selectors Supplement')
    //(S: $F0000; E: $FFFFF; PG: 'Supplementary Private Use Area-A'),
    //(S: $100000; E: $10FFFF; PG: 'Supplementary Private Use Area-B')
    );

implementation

end.

