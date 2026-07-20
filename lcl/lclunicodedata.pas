{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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

type
  TUnicodeBlock = record
    S: longint; //Start
    E: longint; //End
    PG: string[50]; //Page Name
    PL: byte; //Plane
    SC: byte; //Script
  end;

  TPlanes = record
    ID: Byte;
    NM: string[40];
  end;

const
  //https://en.wikipedia.org/wiki/Unicode_block
  MaxPlanes = 6;
  Planes: array[0..MaxPlanes] of TPlanes = (
  (ID: 0; NM: 'Basic Multilingual Plane'),
  (ID: 1; NM: 'Supplementary Multilingual Plane'),
  (ID: 2; NM: 'Supplementary Ideographic Plane'),
  (ID: 3; NM: 'Tertiary Ideographic Plane'),
  (ID: 14; NM: 'Supplementary Special-purpose Plane'),
  (ID: 15; NM: 'Supplementary Private Use Area-A'),
  (ID: 16; NM: 'Supplementary Private Use Area-B')
  );

  //https://www.unicode.org/charts
  MaxScripts = 23;
  Scripts: array[0..MaxScripts] of String[40] = (
    'European Scripts',
    'Modifier Letters',
    'Combining Marks',
    'Miscellaneous',
    'African Scripts',
    'Middle Eastern Scripts',
    'Central Asian Scripts',
    'South Asian Scripts',
    'Southeast Asian Scripts',
    'Indonesian & Philippine Scripts',
    'East Asian Scripts',
    'American Scripts',
    'Notational Systems',
    'Punctuation',
    'Alphanumeric Symbols',
    'Technical Symbols',
    'Numbers & Digits',
    'Mathematical Symbols',
    'Emoji & Pictographs',
    'Other Symbols',
    'Specials',
    'Private Use',
    'Surrogates',
    'Noncharacters in Charts'
    );

  MaxUnicodeBlocks = 338;
  UnicodeBlocks: array[0..MaxUnicodeBlocks] of TUnicodeBlock = (
    (S: $0020; E: $007F; PG: 'Basic Latin'; PL: 0; SC: 0), //Warning this is for charactermapdlg.pas: Full range starts at $0000
    (S: $00A0; E: $00FF; PG: 'Latin-1 Supplement'; PL: 0; SC: 0), //Warning this is for charactermapdlg.pas: Full range starts at $0080
    (S: $0100; E: $017F; PG: 'Latin Extended-A'; PL: 0; SC: 0),
    (S: $0180; E: $024F; PG: 'Latin Extended-B'; PL: 0; SC: 0),
    (S: $0250; E: $02AF; PG: 'IPA Extensions'; PL: 0; SC: 0),
    (S: $02B0; E: $02FF; PG: 'Spacing Modifier Letters'; PL: 0; SC: 1),
    (S: $0300; E: $036F; PG: 'Combining Diacritical Marks'; PL: 0; SC: 2),
    (S: $0370; E: $03FF; PG: 'Greek and Coptic'; PL: 0; SC: 0),
    (S: $0400; E: $04FF; PG: 'Cyrillic'; PL: 0; SC: 0),
    (S: $0500; E: $052F; PG: 'Cyrillic Supplement'; PL: 0; SC: 0),
    (S: $0530; E: $058F; PG: 'Armenian'; PL: 0; SC: 0),
    (S: $0590; E: $05FF; PG: 'Hebrew'; PL: 0; SC: 5),
    (S: $0600; E: $06FF; PG: 'Arabic'; PL: 0; SC: 5),
    (S: $0700; E: $074F; PG: 'Syriac'; PL: 0; SC: 5),
    (S: $0750; E: $077F; PG: 'Arabic Supplement'; PL: 0; SC: 5),
    (S: $0780; E: $07BF; PG: 'Thaana'; PL: 0; SC: 7),
    (S: $07C0; E: $07FF; PG: 'NKo'; PL: 0; SC: 4),
    (S: $0800; E: $083F; PG: 'Samaritan'; PL: 0; SC: 5),
    (S: $0840; E: $085F; PG: 'Mandaic'; PL: 0; SC: 5),
    (S: $0860; E: $086F; PG: 'Syriac Supplement'; PL: 0; SC: 5),
    (S: $0870; E: $089F; PG: 'Arabic Extended-B'; PL: 0; SC: 5),
    (S: $08A0; E: $08FF; PG: 'Arabic Extended-A'; PL: 0; SC: 5),
    (S: $0900; E: $097F; PG: 'Devanagari'; PL: 0; SC: 7),
    (S: $0980; E: $09FF; PG: 'Bengali'; PL: 0; SC: 7),
    (S: $0A00; E: $0A7F; PG: 'Gurmukhi'; PL: 0; SC: 7),
    (S: $0A80; E: $0AFF; PG: 'Gujarati'; PL: 0; SC: 7),
    (S: $0B00; E: $0B7F; PG: 'Oriya'; PL: 0; SC: 7),
    (S: $0B80; E: $0BFF; PG: 'Tamil'; PL: 0; SC: 7),
    (S: $0C00; E: $0C7F; PG: 'Telugu'; PL: 0; SC: 7),
    (S: $0C80; E: $0CFF; PG: 'Kannada'; PL: 0; SC: 7),
    (S: $0D00; E: $0D7F; PG: 'Malayalam'; PL: 0; SC: 7),
    (S: $0D80; E: $0DFF; PG: 'Sinhala'; PL: 0; SC: 7),
    (S: $0E00; E: $0E7F; PG: 'Thai'; PL: 0; SC: 8),
    (S: $0E80; E: $0EFF; PG: 'Lao'; PL: 0; SC: 8),
    (S: $0F00; E: $0FFF; PG: 'Tibetan'; PL: 0; SC: 6),
    (S: $1000; E: $109F; PG: 'Myanmar'; PL: 0; SC: 8),
    (S: $10A0; E: $10FF; PG: 'Georgian'; PL: 0; SC: 0),
    (S: $1100; E: $11FF; PG: 'Hangul Jamo'; PL: 0; SC: 10),
    (S: $1200; E: $137F; PG: 'Ethiopic'; PL: 0; SC: 4),
    (S: $1380; E: $139F; PG: 'Ethiopic Supplement'; PL: 0; SC: 4),
    (S: $13A0; E: $13FF; PG: 'Cherokee'; PL: 0; SC: 11),
    (S: $1400; E: $167F; PG: 'Unified Canadian Aboriginal Syllabics'; PL: 0; SC: 11),
    (S: $1680; E: $169F; PG: 'Ogham'; PL: 0; SC: 0),
    (S: $16A0; E: $16FF; PG: 'Runic'; PL: 0; SC: 0),
    (S: $1700; E: $171F; PG: 'Tagalog'; PL: 0; SC: 9),
    (S: $1720; E: $173F; PG: 'Hanunoo'; PL: 0; SC: 9),
    (S: $1740; E: $175F; PG: 'Buhid'; PL: 0; SC: 9),
    (S: $1760; E: $177F; PG: 'Tagbanwa'; PL: 0; SC: 9),
    (S: $1780; E: $17FF; PG: 'Khmer'; PL: 0; SC: 8),
    (S: $1800; E: $18AF; PG: 'Mongolian'; PL: 0; SC: 6),
    (S: $18B0; E: $18FF; PG: 'Unified Canadian Aboriginal Syllabics Extended'; PL: 0; SC: 11),
    (S: $1900; E: $194F; PG: 'Limbu'; PL: 0; SC: 7),
    (S: $1950; E: $197F; PG: 'Tai Le'; PL: 0; SC: 8),
    (S: $1980; E: $19DF; PG: 'New Tai Lue'; PL: 0; SC: 8),
    (S: $19E0; E: $19FF; PG: 'Khmer Symbols'; PL: 0; SC: 8),
    (S: $1A00; E: $1A1F; PG: 'Buginese'; PL: 0; SC: 9),
    (S: $1A20; E: $1AAF; PG: 'Tai Tham'; PL: 0; SC: 8),
    (S: $1AB0; E: $1AFF; PG: 'Combining Diacritical Marks Extended'; PL: 0; SC: 2),
    (S: $1B00; E: $1B7F; PG: 'Balinese'; PL: 0; SC: 9),
    (S: $1B80; E: $1BBF; PG: 'Sundanese'; PL: 0; SC: 9),
    (S: $1BC0; E: $1BFF; PG: 'Batak'; PL: 0; SC: 9),
    (S: $1C00; E: $1C4F; PG: 'Lepcha'; PL: 0; SC: 7),
    (S: $1C50; E: $1C7F; PG: 'Ol Chiki'; PL: 0; SC: 7),
    (S: $1C80; E: $1C8F; PG: 'Cyrillic Extended-C'; PL: 0; SC: 0),
    (S: $1C90; E: $1CBF; PG: 'Georgian Extended'; PL: 0; SC: 0),
    (S: $1CC0; E: $1CCF; PG: 'Sundanese Supplement'; PL: 0; SC: 9),
    (S: $1CD0; E: $1CFF; PG: 'Vedic Extensions'; PL: 0; SC: 7),
    (S: $1D00; E: $1D7F; PG: 'Phonetic Extensions'; PL: 0; SC: 0),
    (S: $1D80; E: $1DBF; PG: 'Phonetic Extensions Supplement'; PL: 0; SC: 0),
    (S: $1DC0; E: $1DFF; PG: 'Combining Diacritical Marks Supplement'; PL: 0; SC: 2),
    (S: $1E00; E: $1EFF; PG: 'Latin Extended Additional'; PL: 0; SC: 0),
    (S: $1F00; E: $1FFF; PG: 'Greek Extended'; PL: 0; SC: 0),
    (S: $2000; E: $206F; PG: 'General Punctuation'; PL: 0; SC: 13),
    (S: $2070; E: $209F; PG: 'Superscripts and Subscripts'; PL: 0; SC: 1),
    (S: $20A0; E: $20CF; PG: 'Currency Symbols'; PL: 0; SC: 19),
    (S: $20D0; E: $20FF; PG: 'Combining Diacritical Marks for Symbols'; PL: 0; SC: 2),
    (S: $2100; E: $214F; PG: 'Letterlike Symbols'; PL: 0; SC: 17),
    (S: $2150; E: $218F; PG: 'Number Forms'; PL: 0; SC: 16),
    (S: $2190; E: $21FF; PG: 'Arrows'; PL: 0; SC: 17),
    (S: $2200; E: $22FF; PG: 'Mathematical Operators'; PL: 0; SC: 17),
    (S: $2300; E: $23FF; PG: 'Miscellaneous Technical'; PL: 0; SC: 15),
    (S: $2400; E: $243F; PG: 'Control Pictures'; PL: 0; SC: 15),
    (S: $2440; E: $245F; PG: 'Optical Character Recognition'; PL: 0; SC: 15),
    (S: $2460; E: $24FF; PG: 'Enclosed Alphanumerics'; PL: 0; SC: 14),
    (S: $2500; E: $257F; PG: 'Box Drawing'; PL: 0; SC: 17),
    (S: $2580; E: $259F; PG: 'Block Elements'; PL: 0; SC: 17),
    (S: $25A0; E: $25FF; PG: 'Geometric Shapes'; PL: 0; SC: 17),
    (S: $2600; E: $26FF; PG: 'Miscellaneous Symbols'; PL: 0; SC: 18),
    (S: $2700; E: $27BF; PG: 'Dingbats'; PL: 0; SC: 18),
    (S: $27C0; E: $27EF; PG: 'Miscellaneous Mathematical Symbols-A'; PL: 0; SC: 17),
    (S: $27F0; E: $27FF; PG: 'Supplemental Arrows-A'; PL: 0; SC: 17),
    (S: $2800; E: $28FF; PG: 'Braille Patterns'; PL: 0; SC: 12),
    (S: $2900; E: $297F; PG: 'Supplemental Arrows-B'; PL: 0; SC: 17),
    (S: $2980; E: $29FF; PG: 'Miscellaneous Mathematical Symbols-B'; PL: 0; SC: 17),
    (S: $2A00; E: $2AFF; PG: 'Supplemental Mathematical Operators'; PL: 0; SC: 17),
    (S: $2B00; E: $2BFF; PG: 'Miscellaneous Symbols and Arrows'; PL: 0; SC: 19),
    (S: $2C00; E: $2C5F; PG: 'Glagolitic'; PL: 0; SC: 0),
    (S: $2C60; E: $2C7F; PG: 'Latin Extended-C'; PL: 0; SC: 0),
    (S: $2C80; E: $2CFF; PG: 'Coptic'; PL: 0; SC: 4),
    (S: $2D00; E: $2D2F; PG: 'Georgian Supplement'; PL: 0; SC: 0),
    (S: $2D30; E: $2D7F; PG: 'Tifinagh'; PL: 0; SC: 4),
    (S: $2D80; E: $2DDF; PG: 'Ethiopic Extended'; PL: 0; SC: 4),
    (S: $2DE0; E: $2DFF; PG: 'Cyrillic Extended-A'; PL: 0; SC: 0),
    (S: $2E00; E: $2E7F; PG: 'Supplemental Punctuation'; PL: 0; SC: 13),
    (S: $2E80; E: $2EFF; PG: 'CJK Radicals Supplement'; PL: 0; SC: 10),
    (S: $2F00; E: $2FDF; PG: 'Kangxi Radicals'; PL: 0; SC: 10),
    (S: $2FF0; E: $2FFF; PG: 'Ideographic Description Characters'; PL: 0; SC: 10),
    (S: $3000; E: $303F; PG: 'CJK Symbols and Punctuation'; PL: 0; SC: 13),
    (S: $3040; E: $309F; PG: 'Hiragana'; PL: 0; SC: 10),
    (S: $30A0; E: $30FF; PG: 'Katakana'; PL: 0; SC: 10),
    (S: $3100; E: $312F; PG: 'Bopomofo'; PL: 0; SC: 10),
    (S: $3130; E: $318F; PG: 'Hangul Compatibility Jamo'; PL: 0; SC: 10),
    (S: $3190; E: $319F; PG: 'Kanbun'; PL: 0; SC: 10),
    (S: $31A0; E: $31BF; PG: 'Bopomofo Extended'; PL: 0; SC: 10),
    (S: $31C0; E: $31EF; PG: 'CJK Strokes'; PL: 0; SC: 10),
    (S: $31F0; E: $31FF; PG: 'Katakana Phonetic Extensions'; PL: 0; SC: 10),
    (S: $3200; E: $32FF; PG: 'Enclosed CJK Letters and Months'; PL: 0; SC: 14),
    (S: $3300; E: $33FF; PG: 'CJK Compatibility'; PL: 0; SC: 14),
    (S: $3400; E: $4DBF; PG: 'CJK Unified Ideographs Extension A'; PL: 0; SC: 10),
    (S: $4DC0; E: $4DFF; PG: 'Yijing Hexagram Symbols'; PL: 0; SC: 19),
    (S: $4E00; E: $9FFF; PG: 'CJK Unified Ideographs'; PL: 0; SC: 10),
    (S: $A000; E: $A48F; PG: 'Yi Syllables'; PL: 0; SC: 10),
    (S: $A490; E: $A4CF; PG: 'Yi Radicals'; PL: 0; SC: 10),
    (S: $A4D0; E: $A4FF; PG: 'Lisu'; PL: 0; SC: 10),
    (S: $A500; E: $A63F; PG: 'Vai'; PL: 0; SC: 4),
    (S: $A640; E: $A69F; PG: 'Cyrillic Extended-B'; PL: 0; SC: 0),
    (S: $A6A0; E: $A6FF; PG: 'Bamum'; PL: 0; SC: 4),
    (S: $A700; E: $A71F; PG: 'Modifier Tone Letters'; PL: 0; SC: 1),
    (S: $A720; E: $A7FF; PG: 'Latin Extended-D'; PL: 0; SC: 0),
    (S: $A800; E: $A82F; PG: 'Syloti Nagri'; PL: 0; SC: 7),
    (S: $A830; E: $A83F; PG: 'Common Indic Number Forms'; PL: 0; SC: 16),
    (S: $A840; E: $A87F; PG: 'Phags-pa'; PL: 0; SC: 6),
    (S: $A880; E: $A8DF; PG: 'Saurashtra'; PL: 0; SC: 7),
    (S: $A8E0; E: $A8FF; PG: 'Devanagari Extended'; PL: 0; SC: 7),
    (S: $A900; E: $A92F; PG: 'Kayah Li'; PL: 0; SC: 8),
    (S: $A930; E: $A95F; PG: 'Rejang'; PL: 0; SC: 9),
    (S: $A960; E: $A97F; PG: 'Hangul Jamo Extended-A'; PL: 0; SC: 10),
    (S: $A980; E: $A9DF; PG: 'Javanese'; PL: 0; SC: 9),
    (S: $A9E0; E: $A9FF; PG: 'Myanmar Extended-B'; PL: 0; SC: 8),
    (S: $AA00; E: $AA5F; PG: 'Cham'; PL: 0; SC: 8),
    (S: $AA60; E: $AA7F; PG: 'Myanmar Extended-A'; PL: 0; SC: 8),
    (S: $AA80; E: $AADF; PG: 'Tai Viet'; PL: 0; SC: 8),
    (S: $AAE0; E: $AAFF; PG: 'Meetei Mayek Extensions'; PL: 0; SC: 7),
    (S: $AB00; E: $AB2F; PG: 'Ethiopic Extended-A'; PL: 0; SC: 4),
    (S: $AB30; E: $AB6F; PG: 'Latin Extended-E'; PL: 0; SC: 0),
    (S: $AB70; E: $ABBF; PG: 'Cherokee Supplement'; PL: 0; SC: 11),
    (S: $ABC0; E: $ABFF; PG: 'Meetei Mayek'; PL: 0; SC: 7),
    (S: $AC00; E: $D7AF; PG: 'Hangul Syllables'; PL: 0; SC: 10),
    (S: $D7B0; E: $D7FF; PG: 'Hangul Jamo Extended-B'; PL: 0; SC: 10),
    (S: $D800; E: $DB7F; PG: 'High Surrogates'; PL: 0; SC: 22),
    (S: $DB80; E: $DBFF; PG: 'High Private Use Surrogates'; PL: 0; SC: 100),
    (S: $DC00; E: $DFFF; PG: 'Low Surrogates'; PL: 0; SC: 22),
    (S: $E000; E: $F8FF; PG: 'Private Use Area'; PL: 0; SC: 21),
    (S: $F900; E: $FAFF; PG: 'CJK Compatibility Ideographs'; PL: 0; SC: 10),
    (S: $FB00; E: $FB4F; PG: 'Alphabetic Presentation Forms'; PL: 0; SC: 3),
    (S: $FB50; E: $FDFF; PG: 'Arabic Presentation Forms-A'; PL: 0; SC: 5),
    (S: $FE00; E: $FE0F; PG: 'Variation Selectors'; PL: 0; SC: 20),
    (S: $FE10; E: $FE1F; PG: 'Vertical Forms'; PL: 0; SC: 13),
    (S: $FE20; E: $FE2F; PG: 'Combining Half Marks'; PL: 0; SC: 2),
    (S: $FE30; E: $FE4F; PG: 'CJK Compatibility Forms'; PL: 0; SC: 13),
    (S: $FE50; E: $FE6F; PG: 'Small Form Variants'; PL: 0; SC: 13),
    (S: $FE70; E: $FEFF; PG: 'Arabic Presentation Forms-B'; PL: 0; SC: 5),
    (S: $FF00; E: $FFEF; PG: 'Halfwidth and Fullwidth Forms'; PL: 0; SC: 13),
    (S: $FFF0; E: $FFFF; PG: 'Specials'; PL: 0; SC: 20),
    (S: $10000; E: $1007F; PG: 'Linear B Syllabary'; PL: 1; SC: 0),
    (S: $10080; E: $100FF; PG: 'Linear B Ideograms'; PL: 1; SC: 0),
    (S: $10100; E: $1013F; PG: 'Aegean Numbers'; PL: 1; SC: 0),
    (S: $10140; E: $1018F; PG: 'Ancient Greek Numbers'; PL: 1; SC: 0),
    (S: $10190; E: $101CF; PG: 'Ancient Symbols'; PL: 1; SC: 19),
    (S: $101D0; E: $101FF; PG: 'Phaistos Disc'; PL: 1; SC: 0),
    (S: $10280; E: $1029F; PG: 'Lycian'; PL: 1; SC: 0),
    (S: $102A0; E: $102DF; PG: 'Carian'; PL: 1; SC: 0),
    (S: $102E0; E: $102FF; PG: 'Coptic Epact Numbers'; PL: 1; SC: 16),
    (S: $10300; E: $1032F; PG: 'Old Italic'; PL: 1; SC: 0),
    (S: $10330; E: $1034F; PG: 'Gothic'; PL: 1; SC: 0),
    (S: $10350; E: $1037F; PG: 'Old Permic'; PL: 1; SC: 0),
    (S: $10380; E: $1039F; PG: 'Ugaritic'; PL: 1; SC: 5),
    (S: $103A0; E: $103DF; PG: 'Old Persian'; PL: 1; SC: 5),
    (S: $10400; E: $1044F; PG: 'Deseret'; PL: 1; SC: 11),
    (S: $10450; E: $1047F; PG: 'Shavian'; PL: 1; SC: 0),
    (S: $10480; E: $104AF; PG: 'Osmanya'; PL: 1; SC: 4),
    (S: $104B0; E: $104FF; PG: 'Osage'; PL: 1; SC: 11),
    (S: $10500; E: $1052F; PG: 'Elbasan'; PL: 1; SC: 0),
    (S: $10530; E: $1056F; PG: 'Caucasian Albanian'; PL: 1; SC: 0),
    (S: $10570; E: $105BF; PG: 'Vithkuqi'; PL: 1; SC: 0),
    (S: $105C0; E: $105FF; PG: 'Todhri'; PL: 1; SC: 0),
    (S: $10600; E: $1077F; PG: 'Linear A'; PL: 1; SC: 0),
    (S: $10780; E: $107BF; PG: 'Latin Extended-F'; PL: 1; SC: 0),
    (S: $10800; E: $1083F; PG: 'Cypriot Syllabary'; PL: 1; SC: 0),
    (S: $10840; E: $1085F; PG: 'Imperial Aramaic'; PL: 1; SC: 5),
    (S: $10860; E: $1087F; PG: 'Palmyrene'; PL: 1; SC: 5),
    (S: $10880; E: $108AF; PG: 'Nabataean'; PL: 1; SC: 5),
    (S: $108E0; E: $108FF; PG: 'Hatran'; PL: 1; SC: 5),
    (S: $10900; E: $1091F; PG: 'Phoenician'; PL: 1; SC: 5),
    (S: $10920; E: $1093F; PG: 'Lydian'; PL: 1; SC: 0),
    (S: $10980; E: $1099F; PG: 'Meroitic Hieroglyphs'; PL: 1; SC: 4),
    (S: $109A0; E: $109FF; PG: 'Meroitic Cursive'; PL: 1; SC: 4),
    (S: $10A00; E: $10A5F; PG: 'Kharoshthi'; PL: 1; SC: 7),
    (S: $10A60; E: $10A7F; PG: 'Old South Arabian'; PL: 1; SC: 5),
    (S: $10A80; E: $10A9F; PG: 'Old North Arabian'; PL: 1; SC: 5),
    (S: $10AC0; E: $10AFF; PG: 'Manichaean'; PL: 1; SC: 6),
    (S: $10B00; E: $10B3F; PG: 'Avestan'; PL: 1; SC: 5),
    (S: $10B40; E: $10B5F; PG: 'Inscriptional Parthian'; PL: 1; SC: 5),
    (S: $10B60; E: $10B7F; PG: 'Inscriptional Pahlavi'; PL: 1; SC: 5),
    (S: $10B80; E: $10BAF; PG: 'Psalter Pahlavi'; PL: 1; SC: 5),
    (S: $10C00; E: $10C4F; PG: 'Old Turkic'; PL: 1; SC: 6),
    (S: $10C80; E: $10CFF; PG: 'Old Hungarian'; PL: 1; SC: 0),
    (S: $10D00; E: $10D3F; PG: 'Hanifi Rohingya'; PL: 1; SC: 8),
    (S: $10D40; E: $10D8F; PG: 'Garay'; PL: 1; SC: 4),
    (S: $10E60; E: $10E7F; PG: 'Rumi Numeral Symbols'; PL: 1; SC: 16),
    (S: $10E80; E: $10EBF; PG: 'Yezidi'; PL: 1; SC: 5),
    (S: $10EC0; E: $10EFF; PG: 'Arabic Extended-C'; PL: 1; SC: 5),
    (S: $10F00; E: $10F2F; PG: 'Old Sogdian'; PL: 1; SC: 6),
    (S: $10F30; E: $10F6F; PG: 'Sogdian'; PL: 1; SC: 6),
    (S: $10F70; E: $10FAF; PG: 'Old Uyghur'; PL: 1; SC: 6),
    (S: $10FB0; E: $10FDF; PG: 'Chorasmian'; PL: 1; SC: 5),
    (S: $10FE0; E: $10FFF; PG: 'Elymaic'; PL: 1; SC: 5),
    (S: $11000; E: $1107F; PG: 'Brahmi'; PL: 1; SC: 7),
    (S: $11080; E: $110CF; PG: 'Kaithi'; PL: 1; SC: 7),
    (S: $110D0; E: $110FF; PG: 'Sora Sompeng'; PL: 1; SC: 7),
    (S: $11100; E: $1114F; PG: 'Chakma'; PL: 1; SC: 7),
    (S: $11150; E: $1117F; PG: 'Mahajani'; PL: 1; SC: 7),
    (S: $11180; E: $111DF; PG: 'Sharada'; PL: 1; SC: 7),
    (S: $111E0; E: $111FF; PG: 'Sinhala Archaic Numbers'; PL: 1; SC: 16),
    (S: $11200; E: $1124F; PG: 'Khojki'; PL: 1; SC: 7),
    (S: $11280; E: $112AF; PG: 'Multani'; PL: 1; SC: 7),
    (S: $112B0; E: $112FF; PG: 'Khudawadi'; PL: 1; SC: 7),
    (S: $11300; E: $1137F; PG: 'Grantha'; PL: 1; SC: 7),
    (S: $11380; E: $113FF; PG: 'Tulu-Tigalari'; PL: 1; SC: 7),
    (S: $11400; E: $1147F; PG: 'Newa'; PL: 1; SC: 7),
    (S: $11480; E: $114DF; PG: 'Tirhuta'; PL: 1; SC: 7),
    (S: $11580; E: $115FF; PG: 'Siddham'; PL: 1; SC: 7),
    (S: $11600; E: $1165F; PG: 'Modi'; PL: 1; SC: 7),
    (S: $11660; E: $1167F; PG: 'Mongolian Supplement'; PL: 1; SC: 6),
    (S: $11680; E: $116CF; PG: 'Takri'; PL: 1; SC: 7),
    (S: $116D0; E: $116FF; PG: 'Myanmar Extended-C'; PL: 1; SC: 8),
    (S: $11700; E: $1174F; PG: 'Ahom'; PL: 1; SC: 7),
    (S: $11800; E: $1184F; PG: 'Dogra'; PL: 1; SC: 7),
    (S: $118A0; E: $118FF; PG: 'Warang Citi'; PL: 1; SC: 7),
    (S: $11900; E: $1195F; PG: 'Dives Akuru'; PL: 1; SC: 7),
    (S: $119A0; E: $119FF; PG: 'Nandinagari'; PL: 1; SC: 7),
    (S: $11A00; E: $11A4F; PG: 'Zanabazar Square'; PL: 1; SC: 6),
    (S: $11A50; E: $11AAF; PG: 'Soyombo'; PL: 1; SC: 6),
    (S: $11AB0; E: $11ABF; PG: 'Unified Canadian Aboriginal Syllabics Extended-A'; PL: 1; SC: 11),
    (S: $11AC0; E: $11AFF; PG: 'Pau Cin Hau'; PL: 1; SC: 8),
    (S: $11B00; E: $11B5F; PG: 'Devanagari Extended-A'; PL: 1; SC: 7),
    (S: $11BC0; E: $11BFF; PG: 'Sunuwar'; PL: 1; SC: 7),
    (S: $11C00; E: $11C6F; PG: 'Bhaiksuki'; PL: 1; SC: 7),
    (S: $11C70; E: $11CBF; PG: 'Marchen'; PL: 1; SC: 6),
    (S: $11D00; E: $11D5F; PG: 'Masaram Gondi'; PL: 1; SC: 7),
    (S: $11D60; E: $11DAF; PG: 'Gunjala Gondi'; PL: 1; SC: 7),
    (S: $11EE0; E: $11EFF; PG: 'Makasar'; PL: 1; SC: 9),
    (S: $11F00; E: $11F5F; PG: 'Kawi'; PL: 1; SC: 9),
    (S: $11FB0; E: $11FBF; PG: 'Lisu Supplement'; PL: 1; SC: 10),
    (S: $11FC0; E: $11FFF; PG: 'Tamil Supplement'; PL: 1; SC: 7),
    (S: $12000; E: $123FF; PG: 'Cuneiform'; PL: 1; SC: 5),
    (S: $12400; E: $1247F; PG: 'Cuneiform Numbers and Punctuation'; PL: 1; SC: 16),
    (S: $12480; E: $1254F; PG: 'Early Dynastic Cuneiform'; PL: 1; SC: 5),
    (S: $12F90; E: $12FFF; PG: 'Cypro-Minoan'; PL: 1; SC: 0),
    (S: $13000; E: $1342F; PG: 'Egyptian Hieroglyphs'; PL: 1; SC: 4),
    (S: $13430; E: $1345F; PG: 'Egyptian Hieroglyph Format Controls'; PL: 1; SC: 4),
    (S: $13460; E: $143FF; PG: 'Egyptian Hieroglyphs Extended-A'; PL: 1; SC: 4),
    (S: $14400; E: $1467F; PG: 'Anatolian Hieroglyphs'; PL: 1; SC: 5),
    (S: $16100; E: $1613F; PG: 'Gurung Khema'; PL: 1; SC: 7),
    (S: $16800; E: $16A3F; PG: 'Bamum Supplement'; PL: 1; SC: 4),
    (S: $16A40; E: $16A6F; PG: 'Mro'; PL: 1; SC: 7),
    (S: $16A70; E: $16ACF; PG: 'Tangsa'; PL: 1; SC: 8),
    (S: $16AD0; E: $16AFF; PG: 'Bassa Vah'; PL: 1; SC: 4),
    (S: $16B00; E: $16B8F; PG: 'Pahawh Hmong'; PL: 1; SC: 8),
    (S: $16D40; E: $16D7F; PG: 'Kirat Rai'; PL: 1; SC: 7),
    (S: $16E40; E: $16E9F; PG: 'Medefaidrin'; PL: 1; SC: 4),
    (S: $16F00; E: $16F9F; PG: 'Miao'; PL: 1; SC: 10),
    (S: $16FE0; E: $16FFF; PG: 'Ideographic Symbols and Punctuation'; PL: 1; SC: 13),
    (S: $17000; E: $187FF; PG: 'Tangut'; PL: 1; SC: 10),
    (S: $18800; E: $18AFF; PG: 'Tangut Components'; PL: 1; SC: 10),
    (S: $18B00; E: $18CFF; PG: 'Khitan Small Script'; PL: 1; SC: 10),
    (S: $18D00; E: $18D7F; PG: 'Tangut Supplement'; PL: 1; SC: 10),
    (S: $1AFF0; E: $1AFFF; PG: 'Kana Extended-B'; PL: 1; SC: 10),
    (S: $1B000; E: $1B0FF; PG: 'Kana Supplement'; PL: 1; SC: 10),
    (S: $1B100; E: $1B12F; PG: 'Kana Extended-A'; PL: 1; SC: 10),
    (S: $1B130; E: $1B16F; PG: 'Small Kana Extension'; PL: 1; SC: 10),
    (S: $1B170; E: $1B2FF; PG: 'Nushu'; PL: 1; SC: 10),
    (S: $1BC00; E: $1BC9F; PG: 'Duployan'; PL: 1; SC: 12),
    (S: $1BCA0; E: $1BCAF; PG: 'Shorthand Format Controls'; PL: 1; SC: 12),
    (S: $1CC00; E: $1CEBF; PG: 'Symbols for Legacy Computing Supplement'; PL: 1; SC: 19),
    (S: $1CF00; E: $1CFCF; PG: 'Znamenny Musical Notation'; PL: 1; SC: 12),
    (S: $1D000; E: $1D0FF; PG: 'Byzantine Musical Symbols'; PL: 1; SC: 12),
    (S: $1D100; E: $1D1FF; PG: 'Musical Symbols'; PL: 1; SC: 12),
    (S: $1D200; E: $1D24F; PG: 'Ancient Greek Musical Notation'; PL: 1; SC: 12),
    (S: $1D2C0; E: $1D2DF; PG: 'Kaktovik Numerals'; PL: 1; SC: 16),
    (S: $1D2E0; E: $1D2FF; PG: 'Mayan Numerals'; PL: 1; SC: 16),
    (S: $1D300; E: $1D35F; PG: 'Tai Xuan Jing Symbols'; PL: 1; SC: 19),
    (S: $1D360; E: $1D37F; PG: 'Counting Rod Numerals'; PL: 1; SC: 16),
    (S: $1D400; E: $1D7FF; PG: 'Mathematical Alphanumeric Symbols'; PL: 1; SC: 17),
    (S: $1D800; E: $1DAAF; PG: 'Sutton SignWriting'; PL: 1; SC: 12),
    (S: $1DF00; E: $1DFFF; PG: 'Latin Extended-G'; PL: 1; SC: 0),
    (S: $1E000; E: $1E02F; PG: 'Glagolitic Supplement'; PL: 1; SC: 0),
    (S: $1E030; E: $1E08F; PG: 'Cyrillic Extended-D'; PL: 1; SC: 0),
    (S: $1E100; E: $1E14F; PG: 'Nyiakeng Puachue Hmong'; PL: 1; SC: 8),
    (S: $1E290; E: $1E2BF; PG: 'Toto'; PL: 1; SC: 7),
    (S: $1E2C0; E: $1E2FF; PG: 'Wancho'; PL: 1; SC: 7),
    (S: $1E4D0; E: $1E4FF; PG: 'Nag Mundari'; PL: 1; SC: 7),
    (S: $1E5D0; E: $1E5FF; PG: 'Ol Onal'; PL: 1; SC: 7),
    (S: $1E7E0; E: $1E7FF; PG: 'Ethiopic Extended-B'; PL: 1; SC: 4),
    (S: $1E800; E: $1E8DF; PG: 'Mende Kikakui'; PL: 1; SC: 4),
    (S: $1E900; E: $1E95F; PG: 'Adlam'; PL: 1; SC: 4),
    (S: $1EC70; E: $1ECBF; PG: 'Indic Siyaq Numbers'; PL: 1; SC: 16),
    (S: $1ED00; E: $1ED4F; PG: 'Ottoman Siyaq Numbers'; PL: 1; SC: 16),
    (S: $1EE00; E: $1EEFF; PG: 'Arabic Mathematical Alphabetic Symbols'; PL: 1; SC: 17),
    (S: $1F000; E: $1F02F; PG: 'Mahjong Tiles'; PL: 1; SC: 19),
    (S: $1F030; E: $1F09F; PG: 'Domino Tiles'; PL: 1; SC: 19),
    (S: $1F0A0; E: $1F0FF; PG: 'Playing Cards'; PL: 1; SC: 19),
    (S: $1F100; E: $1F1E5; PG: 'Enclosed Alphanumeric Supplement'; PL: 1; SC: 14),
    (S: $1F1E6; E: $1F1FF; PG: 'Regional Indicator Symbol'; PL: 1; SC: 14), //separate
    (S: $1F200; E: $1F2FF; PG: 'Enclosed Ideographic Supplement'; PL: 1; SC: 14),
    (S: $1F300; E: $1F5FF; PG: 'Miscellaneous Symbols and Pictographs'; PL: 1; SC: 18),
    (S: $1F600; E: $1F64F; PG: 'Emoticons'; PL: 1; SC: 18),
    (S: $1F650; E: $1F67F; PG: 'Ornamental Dingbats'; PL: 1; SC: 18),
    (S: $1F680; E: $1F6FF; PG: 'Transport and Map Symbols'; PL: 1; SC: 18),
    (S: $1F700; E: $1F77F; PG: 'Alchemical Symbols'; PL: 1; SC: 19),
    (S: $1F780; E: $1F7FF; PG: 'Geometric Shapes Extended'; PL: 1; SC: 17),
    (S: $1F800; E: $1F8FF; PG: 'Supplemental Arrows-C'; PL: 1; SC: 17),
    (S: $1F900; E: $1F9FF; PG: 'Supplemental Symbols and Pictographs'; PL: 1; SC: 18),
    (S: $1FA00; E: $1FA6F; PG: 'Chess Symbols'; PL: 1; SC: 19),
    (S: $1FA70; E: $1FAFF; PG: 'Symbols and Pictographs Extended-A'; PL: 1; SC: 18),
    (S: $1FB00; E: $1FBFF; PG: 'Symbols for Legacy Computing'; PL: 1; SC: 19),
    (S: $20000; E: $2A6DF; PG: 'CJK Unified Ideographs Extension B'; PL: 2; SC: 10),
    (S: $2A700; E: $2B73F; PG: 'CJK Unified Ideographs Extension C'; PL: 2; SC: 10),
    (S: $2B740; E: $2B81F; PG: 'CJK Unified Ideographs Extension D'; PL: 2; SC: 10),
    (S: $2B820; E: $2CEAF; PG: 'CJK Unified Ideographs Extension E'; PL: 2; SC: 10),
    (S: $2CEB0; E: $2EBEF; PG: 'CJK Unified Ideographs Extension F'; PL: 2; SC: 10),
    (S: $2EBF0; E: $2EE5F; PG: 'CJK Unified Ideographs Extension I'; PL: 2; SC: 10),
    (S: $2F800; E: $2FA1F; PG: 'CJK Compatibility Ideographs Supplement'; PL: 2; SC: 10),
    (S: $30000; E: $3134F; PG: 'CJK Unified Ideographs Extension G'; PL: 3; SC: 10),
    (S: $31350; E: $323AF; PG: 'CJK Unified Ideographs Extension H'; PL: 3; SC: 10),
    (S: $E0000; E: $E007F; PG: 'Tags'; PL: 14; SC: 20),
    (S: $E0100; E: $E01EF; PG: 'Variation Selectors Supplement'; PL: 14; SC: 20),
    (S: $F0000; E: $FFFFF; PG: 'Supplementary Private Use Area-A'; PL: 15; SC: 21),
    (S: $100000; E: $10FFFF; PG: 'Supplementary Private Use Area-B'; PL: 16; SC: 21)
    );

function GetInfoForCodePoint(CP:RawByteString):String;

implementation

uses sysutils, LazUTF8;

function GetInfoForCodePoint(CP:RawByteString):String;
var Cpc:Cardinal;
  BlockIdx, Len:Integer;
begin
  Cpc:=UTF8CodepointToUnicode(@CP[1],Len);
  for BlockIdx:=Low(UnicodeBlocks) to High(UnicodeBlocks) do if
    (UnicodeBlocks[BlockIdx].S <= Cpc) and (UnicodeBlocks[BlockIdx].E >= Cpc) then
    begin
      Result:=Scripts[UnicodeBlocks[BlockIdx].SC]+' | '+Planes[UnicodeBlocks[BlockIdx].PL].NM+' | '+UnicodeBlocks[BlockIdx].PG;
      Exit;
    end;
  Result:=EmptyStr;
end;

end.
