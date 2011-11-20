{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHTML.pas, released 2000-04-10.
The Original Code is based on the hkHTMLSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Hideo Koiso.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an HTML highlighter for SynEdit)
@author(Hideo Koiso, converted to SynEdit by Michael Hieke)
@created(1999-11-02, converted to SynEdit 2000-04-10)
@lastmod(2000-06-23)
The SynHighlighterHTML unit provides SynEdit with an HTML highlighter.
}
unit SynHighlighterHTML;

{$I synedit.inc}

interface

uses
  SysUtils, Classes, Graphics, SynEditTypes, SynEditHighlighter,
  SynEditHighlighterXMLBase, SynEditHighlighterFoldBase;

const
  MAX_ESCAPEAMPS = 151;

  EscapeAmps: array[0..MAX_ESCAPEAMPS - 1] of PChar = (
    ('&amp;'),               {   &   }
    ('&lt;'),                {   >   }
    ('&gt;'),                {   <   }
    ('&quot;'),              {   "   }
    ('&trade;'),             {      }
    ('&nbsp;'),              { space }
    ('&copy;'),              {   ©   }
    ('&reg;'),               {   ®   }
    ('&Agrave;'),            {   À   }
    ('&Aacute;'),            {   Á   }
    ('&Acirc;'),             {   Â   }
    ('&Atilde;'),            {   Ã   }
    ('&Auml;'),              {   Ä   }
    ('&Aring;'),             {   Å   }
    ('&AElig;'),             {   Æ   }
    ('&Ccedil;'),            {   Ç   }
    ('&Egrave;'),            {   È   }
    ('&Eacute;'),            {   É   }
    ('&Ecirc;'),             {   Ê   }
    ('&Euml;'),              {   Ë   }
    ('&Igrave;'),            {   Ì   }
    ('&Iacute;'),            {   Í   }
    ('&Icirc;'),             {   Î   }
    ('&Iuml;'),              {   Ï   }
    ('&ETH;'),               {   Ð   }
    ('&Ntilde;'),            {   Ñ   }
    ('&Ograve;'),            {   Ò   }
    ('&Oacute;'),            {   Ó   }
    ('&Ocirc;'),             {   Ô   }
    ('&Otilde;'),            {   Õ   }
    ('&Ouml;'),              {   Ö   }
    ('&Oslash;'),            {   Ø   }
    ('&Ugrave;'),            {   Ù   }
    ('&Uacute;'),            {   Ú   }
    ('&Ucirc;'),             {   Û   }
    ('&Uuml;'),              {   Ü   }
    ('&Yacute;'),            {   Ý   }
    ('&THORN;'),             {   Þ   }
    ('&szlig;'),             {   ß   }
    ('&agrave;'),            {   à   }
    ('&aacute;'),            {   á   }
    ('&acirc;'),             {   â   }
    ('&atilde;'),            {   ã   }
    ('&auml;'),              {   ä   }
    ('&aring;'),             {   å   }
    ('&aelig;'),             {   æ   }
    ('&ccedil;'),            {   ç   }
    ('&egrave;'),            {   è   }
    ('&eacute;'),            {   é   }
    ('&ecirc;'),             {   ê   }
    ('&euml;'),              {   ë   }
    ('&igrave;'),            {   ì   }
    ('&iacute;'),            {   í   }
    ('&icirc;'),             {   î   }
    ('&iuml;'),              {   ï   }
    ('&eth;'),               {   ð   }
    ('&ntilde;'),            {   ñ   }
    ('&ograve;'),            {   ò   }
    ('&oacute;'),            {   ó   }
    ('&ocirc;'),             {   ô   }
    ('&otilde;'),            {   õ   }
    ('&ouml;'),              {   ö   }
    ('&oslash;'),            {   ø   }
    ('&ugrave;'),            {   ù   }
    ('&uacute;'),            {   ú   }
    ('&ucirc;'),             {   û   }
    ('&uuml;'),              {   ü   }
    ('&yacute;'),            {   ý   }
    ('&thorn;'),             {   þ   }
    ('&yuml;'),              {   ÿ   }
    ('&iexcl;'),             {   ¡   }
    ('&cent;'),              {   ¢   }
    ('&pound;'),             {   £   }
    ('&curren;'),            {   ¤   }
    ('&yen;'),               {   ¥   }
    ('&brvbar;'),            {   ¦   }
    ('&sect;'),              {   §   }
    ('&uml;'),               {   ¨   }
    ('&ordf;'),              {   ª   }
    ('&laquo;'),             {   «   }
    ('&shy;'),               {   ¬   }
    ('&macr;'),              {   ¯   }
    ('&deg;'),               {   °   }
    ('&plusmn;'),            {   ±   }
    ('&sup2;'),              {   ²   }
    ('&sup3;'),              {   ³   }
    ('&acute;'),             {   ´   }
    ('&micro;'),             {   µ   }
    ('&middot;'),            {   ·   }
    ('&cedil;'),             {   ¸   }
    ('&sup1;'),              {   ¹   }
    ('&ordm;'),              {   º   }
    ('&raquo;'),             {   »   }
    ('&frac14;'),            {   ¼   }
    ('&frac12;'),            {   ½   }
    ('&frac34;'),            {   ¾   }
    ('&iquest;'),            {   ¿   }
    ('&times;'),             {   ×   }
    ('&divide'),             {   ÷   }
    ('&euro;'),              {      }
    //used by very old HTML editors
    ('&#9;'),                {  TAB  }
    ('&#127;'),              {      }
    ('&#128;'),              {      }
    ('&#129;'),              {      }
    ('&#130;'),              {      }
    ('&#131;'),              {      }
    ('&#132;'),              {      }
    ('&ldots;'),             {      }
    ('&#134;'),              {      }
    ('&#135;'),              {      }
    ('&#136;'),              {      }
    ('&#137;'),              {      }
    ('&#138;'),              {      }
    ('&#139;'),              {      }
    ('&#140;'),              {      }
    ('&#141;'),              {      }
    ('&#142;'),              {      }
    ('&#143;'),              {      }
    ('&#144;'),              {      }
    ('&#152;'),              {      }
    ('&#153;'),              {      }
    ('&#154;'),              {      }
    ('&#155;'),              {      }
    ('&#156;'),              {      }
    ('&#157;'),              {      }
    ('&#158;'),              {      }
    ('&#159;'),              {      }
    ('&#161;'),              {   ¡   }
    ('&#162;'),              {   ¢   }
    ('&#163;'),              {   £   }
    ('&#164;'),              {   ¤   }
    ('&#165;'),              {   ¥   }
    ('&#166;'),              {   ¦   }
    ('&#167;'),              {   §   }
    ('&#168;'),              {   ¨   }
    ('&#170;'),              {   ª   }
    ('&#175;'),              {   »   }
    ('&#176;'),              {   °   }
    ('&#177;'),              {   ±   }
    ('&#178;'),              {   ²   }
    ('&#180;'),              {   ´   }
    ('&#181;'),              {   µ   }
    ('&#183;'),              {   ·   }
    ('&#184;'),              {   ¸   }
    ('&#185;'),              {   ¹   }
    ('&#186;'),              {   º   }
    ('&#188;'),              {   ¼   }
    ('&#189;'),              {   ½   }
    ('&#190;'),              {   ¾   }
    ('&#191;'),              {   ¿   }
    ('&#215;')               {   Ô   }
  );


type
  TtkTokenKind = (tkAmpersand, tkASP, tkComment, tkIdentifier, tkKey, tkNull,
    tkSpace, tkString, tkSymbol, tkText, tkUndefKey, tkValue);

  TRangeState = (rsAmpersand, rsASP, rsComment, rsKey, rsParam, rsText,
    rsUnKnown, rsValue);

 THtmlCodeFoldBlockType = (
    cfbtHtmlNode,     // <foo>...</node>
    cfbtHtmlComment,  // <!-- -->
    cfbtHtmlAsp,  // <% asp  %>
    // internal types / not configurable
    cfbtHtmlNone
  );

  TProcTableProc = procedure of object;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TSynHTMLSynMode = (shmHtml, shmXHtml);

  { TSynHTMLSyn }

  TSynHTMLSyn = class(TSynCustomXmlHighlighter)
  private
    FMode: TSynHTMLSynMode;
    fSimpleTag: Boolean;
    fAndCode: Integer;
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: Longint;
    Temp: PChar;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..243] of TIdentFuncTableFunc;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fAndAttri: TSynHighlighterAttributes;
    fASPAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fUndefKeyAttri: TSynHighlighterAttributes;
    fValueAttri: TSynHighlighterAttributes;
    fLineNumber: Integer;

    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func1: TtkTokenKind;
    function Func2: TtkTokenKind;
    function Func8: TtkTokenKind;
    function Func9: TtkTokenKind;
    function Func10: TtkTokenKind;
    function Func11: TtkTokenKind;
    function Func12: TtkTokenKind;
    function Func13: TtkTokenKind;
    function Func14: TtkTokenKind;
    function Func16: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func145: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func149: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func151: TtkTokenKind;
    function Func152: TtkTokenKind;
    function Func153: TtkTokenKind;
    function Func154: TtkTokenKind;
    function Func155: TtkTokenKind;
    function Func157: TtkTokenKind;
    function Func159: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func161: TtkTokenKind;
    function Func162: TtkTokenKind;
    function Func163: TtkTokenKind;
    function Func164: TtkTokenKind;
    function Func168: TtkTokenKind;
    function Func169: TtkTokenKind;
    function Func170: TtkTokenKind;
    function Func171: TtkTokenKind;
    function Func172: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func175: TtkTokenKind;
    function Func177: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func179: TtkTokenKind;
    function Func180: TtkTokenKind;
    function Func183: TtkTokenKind;
    function Func186: TtkTokenKind;
    function Func187: TtkTokenKind;
    function Func188: TtkTokenKind;
    function Func192: TtkTokenKind;
    function Func198: TtkTokenKind;
    function Func200: TtkTokenKind;
    function Func202: TtkTokenKind;
    function Func203: TtkTokenKind;
    function Func204: TtkTokenKind;
    function Func205: TtkTokenKind;
    function Func207: TtkTokenKind;
    function Func209: TtkTokenKind;
    function Func211: TtkTokenKind;
    function Func212: TtkTokenKind;
    function Func213: TtkTokenKind;
    function Func214: TtkTokenKind;
    function Func215: TtkTokenKind;
    function Func216: TtkTokenKind;
    function Func227: TtkTokenKind;
    function Func229: TtkTokenKind;
    function Func236: TtkTokenKind;
    function Func243: TtkTokenKind;
    function AltFunc: TtkTokenKind;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure InitIdent;
    procedure MakeMethodTables;
    procedure ASPProc;
    procedure SetMode(const AValue: TSynHTMLSynMode);
    procedure TextProc;
    procedure CommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure AmpersandProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
  protected
    // folding
    procedure CreateRootCodeFoldBlock; override;
    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; override;

    function StartHtmlCodeFoldBlock(ABlockType: THtmlCodeFoldBlockType): TSynCustomCodeFoldBlock;
    function StartHtmlNodeCodeFoldBlock(ABlockType: THtmlCodeFoldBlockType;
                                   OpenPos: Integer; AName: String): TSynCustomCodeFoldBlock;
    procedure EndHtmlNodeCodeFoldBlock(ClosePos: Integer = -1; AName: String = '');
    function TopHtmlCodeFoldBlockType(DownIndex: Integer = 0): THtmlCodeFoldBlockType;

    function GetFoldConfigCount: Integer; override;
    function GetFoldConfigInternalCount: Integer; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}                                         //mh 2000-07-14
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: string; LineNumber:Integer); override;
    function GetToken: string; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    property IdentChars;
  published
    property AndAttri: TSynHighlighterAttributes read fAndAttri write fAndAttri;
    property ASPAttri: TSynHighlighterAttributes read fASPAttri write fASPAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property UndefKeyAttri: TSynHighlighterAttributes read fUndefKeyAttri
      write fUndefKeyAttri;
    property ValueAttri: TSynHighlighterAttributes read fValueAttri
      write fValueAttri;
    property Mode: TSynHTMLSynMode read FMode write SetMode default shmHtml;
  end;

implementation

uses
  SynEditStrConst;

var
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      'a'..'z', 'A'..'Z':
        mHashTable[i] := (Ord(UpCase(i)) - 64);
      '!':
        mHashTable[i] := $7B;
      '/':
        mHashTable[i] := $7A;
      else
        mHashTable[Char(i)] := 0;
    end;
end;

procedure TSynHTMLSyn.InitIdent;
var
  i: Integer;
begin
  for i := 0 to 243 do
    case i of
      1:   fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func1;
      2:   fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func2;
      8:   fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func8;
      9:   fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func9;
      10:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func10;
      11:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func11;
      12:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func12;
      13:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func13;
      14:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func14;
      16:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func16;
      17:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func17;
      18:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func18;
      19:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func19;
      20:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func20;
      21:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func21;
      23:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func23;
      24:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func24;
      25:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func25;
      26:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func26;
      27:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func27;
      28:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func28;
      29:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func29;
      30:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func30;
      31:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func31;
      32:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func32;
      33:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func33;
      35:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func35;
      37:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func37;
      38:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func38;
      39:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func39;
      40:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func40;
      41:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func41;
      42:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func42;
      43:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func43;
      46:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func46;
      47:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func47;
      48:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func48;
      49:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func49;
      50:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func50;
      52:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func52;
      53:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func53;
      55:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func55;
      56:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func56;
      57:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func57;
      58:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func58;
      61:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func61;
      62:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func62;
      64:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func64;
      65:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func65;
      66:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func66;
      67:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func67;
      70:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func70;
      76:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func76;
      78:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func78;
      80:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func80;
      81:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func81;
      82:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func82;
      83:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func83;
      84:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func84;
      85:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func85;
      87:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func87;
      89:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func89;
      90:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func90;
      91:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func91;
      92:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func92;
      93:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func93;
      94:  fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func94;
      105: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func105;
      107: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func107;
      114: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func114;
      121: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func121;
      123: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func123;
      124: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func124;
      130: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func130;
      131: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func131;
      132: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func132;
      133: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func133;
      134: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func134;
      135: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func135;
      136: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func136;
      138: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func138;
      139: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func139;
      140: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func140;
      141: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func141;
      143: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func143;
      145: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func145;
      146: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func146;
      149: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func149;
      150: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func150;
      151: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func151;
      152: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func152;
      153: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func153;
      154: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func154;
      155: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func155;
      157: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func157;
      159: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func159;
      160: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func160;
      161: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func161;
      162: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func162;
      163: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func163;
      164: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func164;
      168: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func168;
      169: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func169;
      170: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func170;
      171: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func171;
      172: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func172;
      174: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func174;
      175: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func175;
      177: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func177;
      178: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func178;
      179: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func179;
      180: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func180;
      183: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func183;
      186: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func186;
      187: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func187;
      188: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func188;
      192: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func192;
      198: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func198;
      200: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func200;
      202: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func202;
      203: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func203;
      204: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func204;
      205: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func205;
      207: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func207;
      209: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func209;
      211: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func211;
      212: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func212;
      213: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func213;
      214: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func214;
      215: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func215;
      216: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func216;
      227: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func227;
      229: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func229;
      236: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func236;
      243: fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}Func243;
      else fIdentFuncTable[i] := {$IFDEF FPC}@{$ENDIF}AltFunc;
    end;
end;

function TSynHTMLSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  While (ToHash^ In ['a'..'z', 'A'..'Z', '!', '/']) do begin
    Inc(Result, mHashTable[ToHash^]);
    Inc(ToHash);
  end;
  While (ToHash^ In ['0'..'9']) do begin
    Inc(Result, (Ord(ToHash^) - Ord('0')) );
    Inc(ToHash);
  end;
  fStringLen := (ToHash - fToIdent);
end;

function TSynHTMLSyn.KeyComp(const aKey: string): Boolean;
var
  i: Integer;
begin
  Temp := fToIdent;
  if (Length(aKey) = fStringLen) then begin
    Result := True;
    For i:=1 To fStringLen do begin
      if (mHashTable[Temp^] <> mHashTable[aKey[i]]) then begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end else begin
    Result := False;
  end;
end;

function TSynHTMLSyn.Func1: TtkTokenKind;
begin
  if KeyComp('A') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func2: TtkTokenKind;
begin
  if KeyComp('B') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func8: TtkTokenKind;
begin
  if KeyComp('DD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func9: TtkTokenKind;
begin
  if KeyComp('I') Or KeyComp('H1') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func10: TtkTokenKind;
begin
  if KeyComp('H2') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func11: TtkTokenKind;
begin
  if KeyComp('H3') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func12: TtkTokenKind;
begin
  if KeyComp('H4') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func13: TtkTokenKind;
begin
  if KeyComp('H5') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func14: TtkTokenKind;
begin
  if KeyComp('H6') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func16: TtkTokenKind;
begin
  if KeyComp('DL') Or KeyComp('P') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func17: TtkTokenKind;
begin
  if KeyComp('KBD') Or KeyComp('Q') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func18: TtkTokenKind;
begin
  if KeyComp('BIG') Or KeyComp('EM') Or KeyComp('HEAD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func19: TtkTokenKind;
begin
  if KeyComp('S') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func20: TtkTokenKind;
begin
  if KeyComp('BR') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func21: TtkTokenKind;
begin
  if KeyComp('DEL') Or KeyComp('LI') Or KeyComp('U') Or KeyComp('BDO') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func23: TtkTokenKind;
begin
  if KeyComp('ABBR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func24: TtkTokenKind;
begin
  if KeyComp('DFN') Or KeyComp('DT') Or KeyComp('TD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func25: TtkTokenKind;
begin
  if KeyComp('AREA') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func26: TtkTokenKind;
begin
  if KeyComp('HR') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func27: TtkTokenKind;
begin
  if KeyComp('BASE') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else if KeyComp('CODE') Or KeyComp('OL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func28: TtkTokenKind;
begin
  if KeyComp('TH') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func29: TtkTokenKind;
begin
  if KeyComp('EMBED') then begin
    Result := tkKey
  end else if KeyComp('IMG') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func30: TtkTokenKind;
begin
  if KeyComp('COL') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else if KeyComp('MAP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func31: TtkTokenKind;
begin
  if KeyComp('DIR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func32: TtkTokenKind;
begin
  if KeyComp('LABEL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func33: TtkTokenKind;
begin
  if KeyComp('UL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func35: TtkTokenKind;
begin
  if KeyComp('DIV') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func37: TtkTokenKind;
begin
  if KeyComp('CITE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func38: TtkTokenKind;
begin
  if KeyComp('THEAD') Or KeyComp('TR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func39: TtkTokenKind;
begin
  if KeyComp('META') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else if KeyComp('PRE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func40: TtkTokenKind;
begin
  if KeyComp('TABLE') Or KeyComp('TT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func41: TtkTokenKind;
begin
  if KeyComp('var') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func42: TtkTokenKind;
begin
  if KeyComp('INS') Or KeyComp('SUB') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func43: TtkTokenKind;
begin
  if KeyComp('FRAME') then begin
    Result := tkKey;
    fSimpleTag := True;
  end
  else if KeyComp('WBR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func46: TtkTokenKind;
begin
  if KeyComp('BODY') then begin
    Result := tkKey;
  end else if KeyComp('LINK') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func47: TtkTokenKind;
begin
  if KeyComp('LEGend') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func48: TtkTokenKind;
begin
  if KeyComp('BLINK') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func49: TtkTokenKind;
begin
  if KeyComp('PARAM') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else if KeyComp('NOBR') Or KeyComp('SAMP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func50: TtkTokenKind;
begin
  if KeyComp('SPAN') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func52: TtkTokenKind;
begin
  if KeyComp('FORM') Or KeyComp('IFRAME') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func53: TtkTokenKind;
begin
  if KeyComp('HTML') Or KeyComp('MENU') Or KeyComp('XMP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func55: TtkTokenKind;
begin
  if KeyComp('FONT') Or KeyComp('object') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func56: TtkTokenKind;
begin
  if KeyComp('SUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func57: TtkTokenKind;
begin
  if KeyComp('SMALL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func58: TtkTokenKind;
begin
  if KeyComp('NOEMBED') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func61: TtkTokenKind;
begin
  if KeyComp('LAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func62: TtkTokenKind;
begin
  if KeyComp('SPACER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func64: TtkTokenKind;
begin
  if KeyComp('SELECT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func65: TtkTokenKind;
begin
  if KeyComp('CENTER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func66: TtkTokenKind;
begin
  if KeyComp('TBODY') Or KeyComp('TITLE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func67: TtkTokenKind;
begin
  if KeyComp('KEYGEN') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func70: TtkTokenKind;
begin
  if KeyComp('ADDRESS') Or KeyComp('APPLET') Or KeyComp('ILAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func76: TtkTokenKind;
begin
  if KeyComp('NEXTID') Or KeyComp('TFOOT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func78: TtkTokenKind;
begin
  if KeyComp('CAPTION') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func80: TtkTokenKind;
begin
  if KeyComp('INPUT') then  begin
    fSimpleTag := True;
    Result := tkKey;
  end else if KeyComp('FIELDSET') Or KeyComp('MARQUEE') then begin
    Result := tkKey;
  end else
    Result := tkUndefKey;
end;

function TSynHTMLSyn.Func81: TtkTokenKind;
begin
  if KeyComp('STYLE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func82: TtkTokenKind;
begin
  if KeyComp('BASEFONT') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else if KeyComp('BGSOUND') Or KeyComp('STRIKE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func83: TtkTokenKind;
begin
  if KeyComp('COMMENT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func84: TtkTokenKind;
begin
  if KeyComp('ISINDEX') then begin
    Result := tkKey;
    fSimpleTag := True;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func85: TtkTokenKind;
begin
  if KeyComp('SCRIPT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func87: TtkTokenKind;
begin
  if KeyComp('SERVER') Or KeyComp('FRAMESET') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func89: TtkTokenKind;
begin
  if KeyComp('ACRONYM') Or KeyComp('OPTION') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func90: TtkTokenKind;
begin
  if KeyComp('LISTING') Or KeyComp('NOLAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func91: TtkTokenKind;
begin
  if KeyComp('NOFRAMES') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func92: TtkTokenKind;
begin
  if KeyComp('BUTTON') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func93: TtkTokenKind;
begin
  if KeyComp('STRONG') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func94: TtkTokenKind;
begin
  if KeyComp('TEXTAREA') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func105: TtkTokenKind;
begin
  if KeyComp('MULTICOL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func107: TtkTokenKind;
begin
  if KeyComp('COLGROUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func114: TtkTokenKind;
begin
  if KeyComp('NOSCRIPT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func121: TtkTokenKind;
begin
  if KeyComp('BLOCKQUOTE') Or KeyComp('PLAINTEXT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func123: TtkTokenKind;
begin
  if KeyComp('/A') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func124: TtkTokenKind;
begin
  if KeyComp('/B') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func130: TtkTokenKind;
begin
  if KeyComp('/DD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func131: TtkTokenKind;
begin
  if KeyComp('/I') Or KeyComp('/H1') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func132: TtkTokenKind;
begin
  if KeyComp('/H2') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func133: TtkTokenKind;
begin
  if KeyComp('/H3') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func134: TtkTokenKind;
begin
  if KeyComp('/H4') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func135: TtkTokenKind;
begin
  if KeyComp('/H5') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func136: TtkTokenKind;
begin
  if KeyComp('/H6') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func138: TtkTokenKind;
begin
  if KeyComp('/DL') Or KeyComp('/P') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func139: TtkTokenKind;
begin
  if KeyComp('/KBD') Or KeyComp('/Q') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func140: TtkTokenKind;
begin
  if KeyComp('/BIG') Or KeyComp('/EM') Or KeyComp('/HEAD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func141: TtkTokenKind;
begin
  if KeyComp('/S') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func143: TtkTokenKind;
begin
  if KeyComp('/DEL') Or KeyComp('/LI') Or KeyComp('/U')  Or KeyComp('/BDO') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func145: TtkTokenKind;
begin
  if KeyComp('/ABBR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func146: TtkTokenKind;
begin
  if KeyComp('/DFN') Or KeyComp('/DT') Or KeyComp('/TD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func149: TtkTokenKind;
begin
  if KeyComp('/CODE') Or KeyComp('/OL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func150: TtkTokenKind;
begin
  if KeyComp('/TH') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func151: TtkTokenKind;
begin
  if KeyComp('/EMBED') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func152: TtkTokenKind;
begin
  if KeyComp('/MAP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func153: TtkTokenKind;
begin
  if KeyComp('/DIR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func154: TtkTokenKind;
begin
  if KeyComp('/LABEL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func155: TtkTokenKind;
begin
  if KeyComp('/UL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func157: TtkTokenKind;
begin
  if KeyComp('/DIV') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func159: TtkTokenKind;
begin
  if KeyComp('/CITE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func160: TtkTokenKind;
begin
  if KeyComp('/THEAD') Or KeyComp('/TR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func161: TtkTokenKind;
begin
  if KeyComp('/PRE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func162: TtkTokenKind;
begin
  if KeyComp('/TABLE') Or KeyComp('/TT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func163: TtkTokenKind;
begin
  if KeyComp('/var') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func164: TtkTokenKind;
begin
  if KeyComp('/INS') Or KeyComp('/SUB') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func168: TtkTokenKind;
begin
  if KeyComp('/BODY') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func169: TtkTokenKind;
begin
  if KeyComp('/LEGend') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func170: TtkTokenKind;
begin
  if KeyComp('/BLINK') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func171: TtkTokenKind;
begin
  if KeyComp('/NOBR') Or KeyComp('/SAMP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func172: TtkTokenKind;
begin
  if KeyComp('/SPAN') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func174: TtkTokenKind;
begin
  if KeyComp('/FORM') Or KeyComp('/IFRAME') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func175: TtkTokenKind;
begin
  if KeyComp('/HTML') Or KeyComp('/MENU') Or KeyComp('/XMP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func177: TtkTokenKind;
begin
  if KeyComp('/FONT') Or KeyComp('/object') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func178: TtkTokenKind;
begin
  if KeyComp('/SUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func179: TtkTokenKind;
begin
  if KeyComp('/SMALL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func180: TtkTokenKind;
begin
  if KeyComp('/NOEMBED') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func183: TtkTokenKind;
begin
  if KeyComp('/LAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func186: TtkTokenKind;
begin
  if KeyComp('/SELECT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func187: TtkTokenKind;
begin
  if KeyComp('/CENTER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func188: TtkTokenKind;
begin
  if KeyComp('/TBODY') Or KeyComp('/TITLE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func192: TtkTokenKind;
begin
  if KeyComp('/ADDRESS') Or KeyComp('/APPLET') Or KeyComp('/ILAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func198: TtkTokenKind;
begin
  if KeyComp('/TFOOT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func200: TtkTokenKind;
begin
  if KeyComp('/CAPTION') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func202: TtkTokenKind;
begin
  if KeyComp('/FIELDSET') Or KeyComp('/MARQUEE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func203: TtkTokenKind;
begin
  if KeyComp('/STYLE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func204: TtkTokenKind;
begin
  if KeyComp('/STRIKE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func205: TtkTokenKind;
begin
  if KeyComp('/COMMENT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func207: TtkTokenKind;
begin
  if KeyComp('/SCRIPT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func209: TtkTokenKind;
begin
  if KeyComp('/FRAMESET') Or KeyComp('/SERVER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func211: TtkTokenKind;
begin
  if KeyComp('/ACRONYM') Or KeyComp('/OPTION') Or KeyComp('!DOCTYPE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func212: TtkTokenKind;
begin
  if KeyComp('/LISTING') Or KeyComp('/NOLAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func213: TtkTokenKind;
begin
  if KeyComp('/NOFRAMES') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func214: TtkTokenKind;
begin
  if KeyComp('/BUTTON') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func215: TtkTokenKind;
begin
  if KeyComp('/STRONG') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func216: TtkTokenKind;
begin
  if KeyComp('/TEXTAREA') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func227: TtkTokenKind;
begin
  if KeyComp('/MULTICOL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func229: TtkTokenKind;
begin
  if KeyComp('/COLGROUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func236: TtkTokenKind;
begin
  if KeyComp('/NOSCRIPT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func243: TtkTokenKind;
begin
  if KeyComp('/BLOCKQUOTE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.AltFunc: TtkTokenKind;
begin
  Result := tkUndefKey;
end;

procedure TSynHTMLSyn.MakeMethodTables;
var
  i: Char;
begin
  For i:=#0 To #255 do begin
    case i of
    #0:
      begin
        fProcTable[i] := {$IFDEF FPC}@{$ENDIF}NullProc;
      end;
    #10:
      begin
        fProcTable[i] := {$IFDEF FPC}@{$ENDIF}LFProc;
      end;
    #13:
      begin
        fProcTable[i] := {$IFDEF FPC}@{$ENDIF}CRProc;
      end;
    #1..#9, #11, #12, #14..#32:
      begin
        fProcTable[i] := {$IFDEF FPC}@{$ENDIF}SpaceProc;
      end;
    '&':
      begin
        fProcTable[i] := {$IFDEF FPC}@{$ENDIF}AmpersandProc;
      end;
    '"':
      begin
        fProcTable[i] := {$IFDEF FPC}@{$ENDIF}StringProc;
      end;
    '<':
      begin
        fProcTable[i] := {$IFDEF FPC}@{$ENDIF}BraceOpenProc;
      end;
    '>':
      begin
        fProcTable[i] := {$IFDEF FPC}@{$ENDIF}BraceCloseProc;
      end;
    '=':
      begin
        fProcTable[i] := {$IFDEF FPC}@{$ENDIF}EqualProc;
      end;
    else
      fProcTable[i] := {$IFDEF FPC}@{$ENDIF}IdentProc;
    end;
  end;
end;

constructor TSynHTMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMode := shmHtml;

  fASPAttri := TSynHighlighterAttributes.Create(SYNS_AttrASP, SYNS_XML_AttrASP);
  fASPAttri.Foreground := clBlack;
  fASPAttri.Background := clYellow;
  AddAttribute(fASPAttri);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  fIdentifierAttri.Style := [fsBold];
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := $00ff0080;
  AddAttribute(fKeyAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  fSymbolAttri.Style := [fsBold];
  AddAttribute(fSymbolAttri);

  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_XML_AttrText);
  AddAttribute(fTextAttri);

  fUndefKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrUnknownWord, SYNS_XML_AttrUnknownWord);
  fUndefKeyAttri.Style := [fsBold];
  fUndefKeyAttri.Foreground := clRed;
  AddAttribute(fUndefKeyAttri);

  fValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue, SYNS_XML_AttrValue);
  fValueAttri.Foreground := $00ff8000;
  AddAttribute(fValueAttri);

  fAndAttri := TSynHighlighterAttributes.Create(SYNS_AttrEscapeAmpersand, SYNS_XML_AttrEscapeAmpersand);
  fAndAttri.Style := [fsBold];
  fAndAttri.Foreground := $0000ff00;
  AddAttribute(fAndAttri);
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);

  InitIdent;
  MakeMethodTables;
  fRange := rsText;
  fDefaultFilter := SYNS_FilterHTML;
end;

procedure TSynHTMLSyn.SetLine(const NewValue: string; LineNumber:Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynHTMLSyn.ASPProc;
begin
  fTokenID := tkASP;
  if (fLine[Run] In [#0, #10, #13]) then begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = '%')
    then begin
      fRange := rsText;
      Inc(Run);
      if TopHtmlCodeFoldBlockType = cfbtHtmlAsp then
        EndHtmlNodeCodeFoldBlock;
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.SetMode(const AValue: TSynHTMLSynMode);
begin
  if FMode = AValue then exit;
  FMode := AValue;
  FAttributeChangeNeedScan := True;
  DefHighlightChange(self);
end;

procedure TSynHTMLSyn.BraceCloseProc;
begin
  fRange := rsText;
  fTokenId := tkSymbol;
  if ((FMode = shmXHtml) or (not fSimpleTag)) and (Run > 0) and (fLine[Run - 1] = '/') then
    EndHtmlNodeCodeFoldBlock(Run + 1, '')
  else
    fSimpleTag := False;
  Inc(Run);
end;

procedure TSynHTMLSyn.CommentProc;
begin
  fTokenID := tkComment;

  if (fLine[Run] In [#0, #10, #13]) then begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = '-') and (fLine[Run - 2] = '-')
    then begin
      fRange := rsText;
      Inc(Run);
      if TopHtmlCodeFoldBlockType = cfbtHtmlComment then
        EndHtmlNodeCodeFoldBlock;
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.BraceOpenProc;
begin
  fSimpleTag := False;
  Inc(Run);
  if (Run <= length(fLine)-2) and (fLine[Run] = '!') and (fLine[Run + 1] = '-') and (fLine[Run + 2] = '-')
  then begin
    fRange := rsComment;
    fTokenID := tkComment;
    StartHtmlCodeFoldBlock(cfbtHtmlComment);
    Inc(Run, 3);
  end
  else if fLine[Run]= '%' then begin
    fRange := rsASP;
    fTokenID := tkASP;
    StartHtmlCodeFoldBlock(cfbtHtmlAsp);
    Inc(Run);
  end
  else begin
    fRange := rsKey;
    fTokenID := tkSymbol;
  end;
end;

procedure TSynHTMLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynHTMLSyn.EqualProc;
begin
  fRange := rsValue;
  fTokenID := tkSymbol;
  Inc(Run);
end;

function TSynHTMLSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  hashKey: Integer;
begin
  fToIdent := MayBe;
  hashKey := KeyHash(MayBe);
  if (hashKey < 244) then begin
    Result := fIdentFuncTable[hashKey]{$IFDEF FPC}(){$ENDIF};
  end else begin
    Result := tkIdentifier;
  end;
end;

procedure TSynHTMLSyn.IdentProc;
begin
  case fRange of
  rsKey:
    begin
      fRange := rsParam;
      fTokenID := IdentKind((fLine + Run));
      if ((FMode = shmXHtml) or (not fSimpleTag)) then
        if fLine[Run] = '/' then
          EndHtmlNodeCodeFoldBlock(Run+1, copy(fline, Run+2, fStringLen-1))
        else if fLine[Run] <> '!' then
          StartHtmlNodeCodeFoldBlock(cfbtHtmlNode, Run, copy(fline, Run+1, fStringLen));
      Inc(Run, fStringLen);
    end;
  rsValue:
    begin
      fRange := rsParam;
      fTokenID := tkValue;
      repeat
        Inc(Run);
      until (fLine[Run] In [#0..#32, '>']);
    end;
  else
    fTokenID := tkIdentifier;
    repeat
      Inc(Run);
    until (fLine[Run] In [#0..#32, '=', '"', '>']);
  end;
end;

procedure TSynHTMLSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynHTMLSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynHTMLSyn.TextProc;
const StopSet = [#0..#31, '<', '&'];
var
  i: Integer;
begin
  if fLine[Run] in (StopSet - ['&']) then begin
    fProcTable[fLine[Run]];
    exit;
  end;

  fTokenID := tkText;
  While True do begin
    while not (fLine[Run] in StopSet) do Inc(Run);

    if (fLine[Run] = '&') then begin
      For i:=Low(EscapeAmps) To High(EscapeAmps) do begin
        if (StrLIComp((fLine + Run), PChar(EscapeAmps[i]), StrLen(EscapeAmps[i])) = 0) then begin
          fAndCode := i;
          fRange := rsAmpersand;
          Exit;
        end;
      end;

      Inc(Run);
    end else begin
      Break;
    end;
  end;

end;

procedure TSynHTMLSyn.AmpersandProc;
begin
  case fAndCode of
  Low(EscapeAmps)..High(EscapeAmps):
    begin
      fTokenID := tkAmpersand;
      Inc(Run, StrLen(EscapeAmps[fAndCode]));
    end;
  end;
  fAndCode := -1;
  fRange := rsText;
end;

procedure TSynHTMLSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while fLine[Run] <= #32 do begin
    if fLine[Run] in [#0, #9, #10, #13] then break;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.StringProc;
begin
  if (fRange = rsValue) then begin
    fRange := rsParam;
    fTokenID := tkValue;
  end else begin
    fTokenID := tkString;
  end;
  Inc(Run);  // first '"'
  while not (fLine[Run] in [#0, #10, #13, '"']) do Inc(Run);
  if fLine[Run] = '"' then Inc(Run);  // last '"'
end;

procedure TSynHTMLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
  rsText:
    begin
      TextProc;
    end;
  rsComment:
    begin
      CommentProc;
    end;
  rsASP:
    begin
      ASPProc;
    end;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynHTMLSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynHTMLSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynHTMLSyn.GetToken: string;
var
  len: Longint;
begin
  Result := '';
  Len := (Run - fTokenPos);
  SetString(Result, (FLine + fTokenPos), len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynHTMLSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;
{$ENDIF}

function TSynHTMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynHTMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkAmpersand: Result := fAndAttri;
    tkASP: Result := fASPAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fValueAttri;
    tkSymbol: Result := fSymbolAttri;
    tkText: Result := fTextAttri;
    tkUndefKey: Result := fUndefKeyAttri;
    tkValue: Result := fValueAttri;
    else Result := nil;
  end;
end;

function TSynHTMLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynHTMLSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynHTMLSyn.GetRange: Pointer;
begin
  CodeFoldRange.RangeType:=Pointer(PtrUInt(Integer(fRange)));
  Result := inherited;
end;

procedure TSynHTMLSyn.SetRange(Value: Pointer);
begin
  inherited;
  fRange := TRangeState(Integer(PtrUInt(CodeFoldRange.RangeType)));
end;

procedure TSynHTMLSyn.ReSetRange;
begin
  inherited;
  fRange:= rsText;
end;

function TSynHTMLSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['0'..'9', 'a'..'z', 'A'..'Z'];
end;

procedure TSynHTMLSyn.CreateRootCodeFoldBlock;
begin
  inherited CreateRootCodeFoldBlock;
  RootCodeFoldBlock.InitRootBlockType(Pointer(PtrInt(cfbtHtmlNone)));
end;

function TSynHTMLSyn.GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
begin
  Result := inherited GetFoldConfigInstance(Index);
  Result.Enabled := True;
end;

function TSynHTMLSyn.StartHtmlCodeFoldBlock(ABlockType: THtmlCodeFoldBlockType): TSynCustomCodeFoldBlock;
begin
  Result := inherited StartXmlCodeFoldBlock(ord(ABlockType));
end;

function TSynHTMLSyn.StartHtmlNodeCodeFoldBlock(ABlockType: THtmlCodeFoldBlockType;
  OpenPos: Integer; AName: String): TSynCustomCodeFoldBlock;
begin
  if not FFoldConfig[ord(cfbtHtmlNode)].Enabled then exit;
  Result := inherited StartXmlNodeCodeFoldBlock(ord(ABlockType), OpenPos, AName);
end;

procedure TSynHTMLSyn.EndHtmlNodeCodeFoldBlock(ClosePos: Integer; AName: String);
begin
  if not FFoldConfig[ord(cfbtHtmlNode)].Enabled then exit;
  inherited EndXmlNodeCodeFoldBlock(ClosePos, AName);
end;

function TSynHTMLSyn.TopHtmlCodeFoldBlockType(DownIndex: Integer): THtmlCodeFoldBlockType;
begin
  Result := THtmlCodeFoldBlockType(PtrUInt(TopCodeFoldBlockType(DownIndex)));
end;

function TSynHTMLSyn.GetFoldConfigCount: Integer;
begin
  // excluded cfbtHtmlNone;
  Result := ord(high(THtmlCodeFoldBlockType)) - ord(low(THtmlCodeFoldBlockType));
end;

function TSynHTMLSyn.GetFoldConfigInternalCount: Integer;
begin
  // include cfbtHtmlNone;
  Result := ord(high(THtmlCodeFoldBlockType)) - ord(low(THtmlCodeFoldBlockType)) + 1;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}                                             //mh 2000-07-14
function TSynHTMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangHTML;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}                                                            //mh 2000-07-14
  RegisterPlaceableHighlighter(TSynHTMLSyn);
{$ENDIF}
end.

