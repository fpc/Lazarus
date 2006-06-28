{******************************************************************}
{*     IPHTML.PAS - HTML Browser and associated classes           *}
{******************************************************************}

{ $Id$ }

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurbºoPower Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * adem baba <adembaba@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{ Global defines potentially affecting this unit }
{$I IPDEFINE.INC}

unit IpHtml;

interface

uses
  {$IFDEF IP_LAZARUS}
  //MemCheck,
  Types,
  LCLType,
  LCLPRoc,
  GraphType,
  LCLIntf,
  LResources,
  LMessages,
  LCLMemManager,
  {$ELSE}
  Windows,
  {$ENDIF}
  Messages,
  SysUtils,
  Classes,
  Graphics,
  {$IFDEF UseGifImageUnit}
    GifImage,
  {$ELSE}
    IpAnim,
    {$IFDEF AndersGIFImage }
      IpAnAGif,
    {$ENDIF}
    {$IFDEF ImageLibGIFImage }
      IpAnImgL,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF UsePNGGraphic}
    IpPNGImg,
  {$ENDIF}      
  Controls,
  StdCtrls,
  ExtCtrls,
  Buttons,
  Forms,
  ClipBrd,
  IpConst,
  IpStrms,
  IpUtils,
  Dialogs,
  IpMsg,
  TypInfo; {!!.10}

type
  {Note: Some of the code below relies on the fact that
   the end tag (when present) immediately follows the
   start tag.}
  TIpHtmlToken = (
    IpHtmlTagEof,
    IpHtmlTagUnknown, IpHtmlTagText,
    IpHtmlTagHtml, IpHtmlTagHtmlend,
    IpHtmlTagHEAD, IpHtmlTagHEADend,
    IpHtmlTagTITLE, IpHtmlTagTITLEend,
    IpHtmlTagSTYLE, IpHtmlTagSTYLEend,
    IpHtmlTagSCRIPT, IpHtmlTagSCRIPTend,
    IpHtmlTagNOSCRIPT, IpHtmlTagNOSCRIPTend,
    IpHtmlTagISINDEX,
    IpHtmlTagBASE,
    IpHtmlTagMETA,
    IpHtmlTagLINK,
    IpHtmlTagBODY, IpHtmlTagBODYend,
    IpHtmlTagH1, IpHtmlTagH1end,
    IpHtmlTagH2, IpHtmlTagH2end,
    IpHtmlTagH3, IpHtmlTagH3end,
    IpHtmlTagH4, IpHtmlTagH4end,
    IpHtmlTagH5, IpHtmlTagH5end,
    IpHtmlTagH6, IpHtmlTagH6end,
    IpHtmlTagFONT, IpHtmlTagFONTend,
    IpHtmlTagP, IpHtmlTagPend,
    IpHtmlTagUL, IpHtmlTagULen,
    IpHtmlTagOL, IpHtmlTagOLend,
    IpHtmlTagLI, IpHtmlTagLIend,
    IpHtmlTagDL, IpHtmlTagDLend,
    IpHtmlTagDT, IpHtmlTagDTend,
    IpHtmlTagDD, IpHtmlTagDDend,
    IpHtmlTagDIR, IpHtmlTagDIRend,
    IpHtmlTagMENU, IpHtmlTagMENUend,
    IpHtmlTagPRE, IpHtmlTagPREend,
    IpHtmlTagDIV, IpHtmlTagDIVend,
    IpHtmlTagSPAN, IpHtmlTagSPANend,
    IpHtmlTagCENTER, IpHtmlTagCENTERend,
    IpHtmlTagLEFT, IpHtmlTagLEFTend,
    IpHtmlTagRIGHT, IpHtmlTagRIGHTend,
    IpHtmlTagBLINK, IpHtmlTagBLINKend,
    IpHtmlTagBLOCKQUOTE, IpHtmlTagBLOCKQUOTEend,
    IpHtmlTagQ, IpHtmlTagQend,
    IpHtmlTagHR,
    IpHtmlTagTT, IpHtmlTagTTend,
    IpHtmlTagI, IpHtmlTagIend,
    IpHtmlTagB, IpHtmlTagBend,
    IpHtmlTagU, IpHtmlTagUend,
    IpHtmlTagSTRIKE, IpHtmlTagSTRIKEend,
    IpHtmlTagS, IpHtmlTagSend,
    IpHtmlTagBIG, IpHtmlTagBIGend,
    IpHtmlTagSMALL, IpHtmlTagSMALLend,
    IpHtmlTagSUB, IpHtmlTagSUBend,
    IpHtmlTagSUP, IpHtmlTagSUPend,
    IpHtmlTagEM, IpHtmlTagEMend,
    IpHtmlTagSTRONG, IpHtmlTagSTRONGend,
    IpHtmlTagDFN, IpHtmlTagDFNend,
    IpHtmlTagCODE, IpHtmlTagCODEend,
    IpHtmlTagSAMP, IpHtmlTagSAMPend,
    IpHtmlTagKBD, IpHtmlTagKBDend,
    IpHtmlTagVAR, IpHtmlTagVARend,
    IpHtmlTagCITE, IpHtmlTagCITEend,
    IpHtmlTagABBR, IpHtmlTagABBRend,
    IpHtmlTagACRONYM, IpHtmlTagACRONYMend,
    IpHtmlTagA, IpHtmlTagAend,
    IpHtmlTagIMG,
    IpHtmlTagAPPLET, IpHtmlTagAPPLETend,
    IpHtmlTagOBJECT, IpHtmlTagOBJECTend,
    IpHtmlTagPARAM,
    IpHtmlTagBASEFONT,
    IpHtmlTagBR,
    IpHtmlTagNOBR, IpHtmlTagNOBRend,
    IpHtmlTagMAP, IpHtmlTagMAPend,
    IpHtmlTagAREA,
    IpHtmlTagDOCTYPE,
    IpHtmlTagCOMMENT,
    IpHtmlTagADDRESS, IpHtmlTagADDRESSend,
    IpHtmlTagFORM, IpHtmlTagFORMend,
    IpHtmlTagTABLE, IpHtmlTagTABLEend,
    IpHtmlTagCAPTION, IpHtmlTagCAPTIONend,
    IpHtmlTagTR, IpHtmlTagTRend,
    IpHtmlTagTH, IpHtmlTagTHend,
    IpHtmlTagTD, IpHtmlTagTDend,
    IpHtmlTagTBODY, IpHtmlTagTBODYend,
    IpHtmlTagTHEAD, IpHtmlTagTHEADend,
    IpHtmlTagTFOOT, IpHtmlTagTFOOTend,
    IpHtmlTagCOLGROUP, IpHtmlTagCOLGROUPend,
    IpHtmlTagCOL,
    IpHtmlTagINPUT,
    IpHtmlTagBUTTON, IpHtmlTagBUTTONend,
    IpHtmlTagSELECT, IpHtmlTagSELECTend,
    IpHtmlTagOPTGROUP, IpHtmlTagOPTGROUPend,
    IpHtmlTagOPTION, IpHtmlTagOPTIONend,
    IpHtmlTagTEXTAREA, IpHtmlTagTEXTAREAend,
    IpHtmlTagLABEL, IpHtmlTagLABELend,
    IpHtmlTagFIELDSET, IpHtmlTagFIELDSETend,
    IpHtmlTagLEGEND, IpHtmlTagLEGENDend,
    IpHtmlTagINS, IpHtmlTagINSend,
    IpHtmlTagDEL, IpHtmlTagDELend,
    IpHtmlTagFRAMESET, IpHtmlTagFRAMESETend,
    IpHtmlTagFRAME,
    IpHtmlTagNOFRAMES, IpHtmlTagNOFRAMESend,
    IpHtmlTagIFRAME, IpHtmlTagIFRAMEend
    );
  TIpHtmlTokenSet = set of TIpHtmlToken;
const
  IpEndTokenSet : TIpHtmlTokenSet = [
    IpHtmlTagHtmlend,
    IpHtmlTagHEADend,
    IpHtmlTagTITLEend,
    IpHtmlTagSTYLEend,
    IpHtmlTagSCRIPTend,
    IpHtmlTagNOSCRIPTend,
    IpHtmlTagBODYend,
    IpHtmlTagH1end,
    IpHtmlTagH2end,
    IpHtmlTagH3end,
    IpHtmlTagH4end,
    IpHtmlTagH5end,
    IpHtmlTagH6end,
    IpHtmlTagFONTend,
    IpHtmlTagPend,
    IpHtmlTagULen,
    IpHtmlTagOLend,
    IpHtmlTagLIend,
    IpHtmlTagDLend,
    IpHtmlTagDDend,
    IpHtmlTagDIRend,
    IpHtmlTagMENUend,
    IpHtmlTagPREend,
    IpHtmlTagDIVend,
    IpHtmlTagSPANend,
    IpHtmlTagCENTERend,
    IpHtmlTagLEFTend,
    IpHtmlTagRIGHTend,
    IpHtmlTagBLINKend,
    IpHtmlTagBLOCKQUOTEend,
    IpHtmlTagQend,
    IpHtmlTagTTend,
    IpHtmlTagIend,
    IpHtmlTagBend,
    IpHtmlTagUend,
    IpHtmlTagSTRIKEend,
    IpHtmlTagSend,
    IpHtmlTagBIGend,
    IpHtmlTagSMALLend,
    IpHtmlTagSUBend,
    IpHtmlTagSUPend,
    IpHtmlTagEMend,
    IpHtmlTagSTRONGend,
    IpHtmlTagDFNend,
    IpHtmlTagCODEend,
    IpHtmlTagSAMPend,
    IpHtmlTagKBDend,
    IpHtmlTagVARend,
    IpHtmlTagCITEend,
    IpHtmlTagABBRend,
    IpHtmlTagACRONYMend,
    IpHtmlTagAend,
    IpHtmlTagAPPLETend,
    IpHtmlTagOBJECTend,
    IpHtmlTagNOBRend,
    IpHtmlTagMAPend,
    IpHtmlTagADDRESSend,
    IpHtmlTagFORMend,
    IpHtmlTagTABLEend,
    IpHtmlTagCAPTIONend,
    IpHtmlTagTRend,
    IpHtmlTagTHend,
    IpHtmlTagTDend,
    IpHtmlTagTBODYend,
    IpHtmlTagTHEADend,
    IpHtmlTagTFOOTend,
    IpHtmlTagCOLGROUPend,
    IpHtmlTagBUTTONend,
    IpHtmlTagSELECTend,
    IpHtmlTagOPTGROUPend,
    IpHtmlTagOPTIONend,
    IpHtmlTagTEXTAREAend,
    IpHtmlTagLABELend,
    IpHtmlTagFIELDSETend,
    IpHtmlTagLEGENDend,
    IpHtmlTagINSend,
    IpHtmlTagDELend,
    IpHtmlTagFRAMESETend,
    IpHtmlTagNOFRAMESend,
    IpHtmlTagIFRAMEend
  ];
  IpHtmlTokens : array[TIpHtmlToken] of PAnsiChar = (
  '<eof>',
  '<unknown>',
  '<text>',
  'HTML', '/HTML',
  'HEAD', '/HEAD',
  'TITLE', '/TITLE',
  'STYLE', '/STYLE',
  'SCRIPT', '/SCRIPT',
  'NOSCRIPT','/NOSCRIPT',
  'ISINDEX',
  'BASE',
  'META',
  'LINK',
  'BODY','/BODY',
  'H1','/H1',
  'H2','/H2',
  'H3','/H3',
  'H4','/H4',
  'H5','/H5',
  'H6','/H6',
  'FONT', '/FONT',
  'P','/P',
  'UL','/UL',
  'OL','/OL',
  'LI','/LI',
  'DL','/DL',
  'DT', '/DT',
  'DD', '/DD',
  'DIR','/DIR',
  'MENU','/MENU',
  'PRE','/PRE',
  'DIV','/DIV',
  'SPAN','/SPAN',
  'CENTER','/CENTER',
  'LEFT', '/LEFT',
  'RIGHT', '/RIGHT',
  'BLINK', '/BLINK',
  'BLOCKQUOTE','/BLOCKQUOTE',
  'Q', '/Q',
  'HR',
  'TT', '/TT',
  'I', '/I',
  'B', '/B',
  'U', '/U',
  'STRIKE', '/STRIKE',
  'S', '/S',
  'BIG', '/BIG',
  'SMALL', '/SMALL',
  'SUB', '/SUB',
  'SUP', '/SUP',
  'EM', '/EM',
  'STRONG', '/STRONG',
  'DFN', '/DFN',
  'CODE', '/CODE',
  'SAMP', '/SAMP',
  'KBD', '/KBD',
  'VAR', '/VAR',
  'CITE', '/CITE',
  'ABBR','/ABBR',
  'ACRONYM','/ACRONYM',
  'A', '/A',
  'IMG',
  'APPLET','/APPLET',
  'OBJECT','/OBJECT',
  'PARAM',
  'BASEFONT',
  'BR',
  'NOBR', '/NOBR',
  'MAP','/MAP',
  'AREA',
  '!DOCTYPE',
  '!--',
  'ADDRESS','/ADDRESS',
  'FORM','/FORM',
  'TABLE','/TABLE',
  'CAPTION','/CAPTION',
  'TR','/TR',
  'TH','/TH',
  'TD','/TD',
  'TBODY','/TBODY',
  'THEAD','/THEAD',
  'TFOOT','/TFOOT',
  'COLGROUP','/COLGROUP',
  'COL',
  'INPUT',
  'BUTTON', '/BUTTON',
  'SELECT','/SELECT',
  'OPTGROUP','/OPTGROUP',
  'OPTION', '/OPTION',
  'TEXTAREA','/TEXTAREA',
  'LABEL','/LABEL',
  'FIELDSET','/FIELDSET',
  'LEGEND','/LEGEND',
  'INS','/INS',
  'DEL','/DEL',
  'FRAMESET','/FRAMESET',
  'FRAME',
  'NOFRAMES','/NOFRAMES',
  'IFRAME', '/IFRAME'
  );
const
  IPMAXFRAMES = 256; {maximum number of frames in a single frameset}
  MAXINTS = 4096; {buffer size - this should be way more than needed}
  TINTARRGROWFACTOR = 64;
  DEFAULT_PRINTMARGIN = 0.5; {inches}                                  {!!.10}
type
  {$IFDEF IP_LAZARUS}
  TIpEnumItemsMethod = TLCLEnumItemsMethod;
  TIpHtmlPoolManager = class(TLCLNonFreeMemManager)
  public
    constructor Create(TheItemSize, MaxItems : DWord);
    function NewItm : Pointer;
  end;
  {$ELSE}
  TIpEnumItemsMethod = procedure(Item: Pointer) of object;
  TIpHtmlPoolManager = class
  protected
    Root : Pointer;
    {Top : Pointer;}                                                   {!!.12}
    NextPage : Pointer;
    Next : Pointer;
    InternalSize : DWord;
    Critical : TRtlCriticalSection;
  protected
    procedure Grow;
  public
    constructor Create(ItemSize, MaxItems : DWord);
    destructor Destroy; override;
    function NewItm : Pointer;
    procedure EnumerateItems(Method: TIpEnumItemsMethod);
  end;
  {$ENDIF}

  TIpHtml = class;
  TIpHtmlAlign = (haDefault, haLeft, haCenter, haRight, haJustify, haChar);
  TIpHtmlVAlign = (hvaTop, hvaMiddle, hvaBottom);
  TIpHtmlVAlign3 = (hva3Top, hva3Middle, hva3Bottom, hva3Baseline, hva3Default);

  TIpHtmlInteger = class(TPersistent)
  {!!.10 new - integer property which can be scaled}
  protected
    FValue : Integer;
    FChange: TNotifyEvent;
    procedure DoChange;
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
    {$IFDEF IP_LAZARUS}
  public
    constructor Create(AValue: Integer);
    {$ENDIF}
  published {public} {!!.12}
    {$IFNDEF IP_LAZARUS}
    constructor Create(AValue: Integer);
    {$EndIf}
    property Value: Integer read GetValue write SetValue;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlPixelsType = (hpUndefined, hpAbsolute);
  TIpHtmlPixels = class(TPersistent)
  protected
    FValue : Integer;
    FPixelsType : TIpHtmlPixelsType;
    FChange: TNotifyEvent;
    procedure DoChange;
    function GetValue: Integer;
    procedure SetPixelsType(const Value: TIpHtmlPixelsType);
    procedure SetValue(const Value: Integer); {record}                          {!!.10}
  published {public} {!!.12}
    property Value: Integer read GetValue write SetValue;
    property PixelsType: TIpHtmlPixelsType read FPixelsType write SetPixelsType;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlLengthType = (hlUndefined, hlAbsolute, hlPercent);
  TIpHtmlLength = class(TPersistent)
  protected
    FLengthValue: Integer;
    FLengthType: TIpHtmlLengthType;
    FChange: TNotifyEvent;
    procedure SetLengthType(const Value: TIpHtmlLengthType);
    procedure SetLengthValue(const Value: Integer);
    function GetLengthValue: Integer;{record}                           {!!.10}
    procedure DoChange;
  published
    property LengthValue : Integer read GetLengthValue write SetLengthValue;
    property LengthType : TIpHtmlLengthType read FLengthType write SetLengthType;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlMultiLengthType = (hmlUndefined, hmlAbsolute, hmlPercent, hmlRelative);
  TIpHtmlMultiLength = class(TPersistent)
  protected
    FLengthValue : Integer;
    FLengthType : TIpHtmlMultiLengthType;
    function GetLengthValue: Integer;{record}                           {!!.10}
  published
    property LengthValue: Integer read GetLengthValue write FLengthValue;
    property LengthType: TIpHtmlMultiLengthType read FLengthType write FLengthType;
  end;

  TIpHtmlMultiLengthList = class(TPersistent)
  protected
    {Entries : Integer;}                                               {!!.10}
    {Values : array[0..pred(IPMAXFRAMES)] of TIpHtmlMultiLength;}      {!!.10}
    List: TList;                                                       {!!.10}
    function GetEntries: Integer;
    function GetValues(Index: Integer): TIpHtmlMultiLength;{record}    {!!.10}
  public
    constructor Create;
    destructor Destroy; override;
    property Values[Index: Integer]: TIpHtmlMultiLength read GetValues;
    procedure AddEntry(Value: TIpHtmlMultiLength);
    procedure Clear;
  published
    property Entries: Integer read GetEntries;
  end;

  TIpHtmlRelSizeType = (hrsUnspecified, hrsAbsolute, hrsRelative);     {!!.10}
  TIpHtmlRelSize = class(TPersistent)
  private
    FChange: TNotifyEvent;
    procedure SetSizeType(const Value: TIpHtmlRelSizeType);
    procedure SetValue(const Value: Integer); {record}                         {!!.10}
  protected
    FValue : Integer;
    FSizeType : TIpHtmlRelSizeType;
    procedure DoChange;
  published
    property Value : Integer read FValue write SetValue;
    property SizeType : TIpHtmlRelSizeType read FSizeType write SetSizeType;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlNode = class;
  TIpHtmlNodeBlock = class;

  {display properties that affect the font size}
  TIpHtmlPropA = class
  protected
    FKnownSizeOfSpace: TSize;
    FBaseFontSize: integer;
    FFontSize: integer;
    FFontName: string;
    FFontStyle: TFontStyles;
    FUseCount: Integer;
    FSizeOfSpaceKnown : Boolean;
  public
    KnownSizeOfHyphen : TSize;
    tmAscent,
    tmDescent,
    tmHeight : Integer;
    procedure SetBaseFontSize(const Value: integer);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: integer);
    procedure SetFontStyle(const Value: TFontStyles);
    property SizeOfSpaceKnown: Boolean read FSizeOfSpaceKnown;
    procedure SetKnownSizeOfSpace(const Size:TSize);
    property KnownSizeOfSpace : TSize read FKnownSizeOfSpace;
    property BaseFontSize : integer read FBaseFontSize
      write SetBaseFontSize;
    property FontName : string read FFontName
      write SetFontName;
    property FontSize : integer read FFontSize
      write SetFontSize;
    property FontStyle : TFontStyles read FFontStyle
      write SetFontStyle;
    property UseCount : Integer read FUseCount
      write FUseCount;
    procedure Assign(const Source: TIpHtmlPropA);
    procedure DecUse;
    procedure IncUse;
    constructor CreateCopy(Source: TIpHtmlPropA);
  end;

  {display properties that don't affect the font size}
  TIpHtmlPropB = class
  protected
    FFontBaseline: integer;
    FAlignment: TIpHtmlAlign;
    FFontColor: TColor;
    FVAlignment: TIpHtmlVAlign3;
    FLinkColor : TColor;
    FVLinkColor : TColor;
    FALinkColor : TColor;
    FBgColor : TColorRef;
    FPreformatted : Boolean;
    FNoBreak : Boolean;
    FUseCount: Integer;
    FOwner: TIpHtml;
  public
    property FontBaseline : integer read FFontBaseline write FFontBaseline;
    property FontColor : TColor read FFontColor write FFontColor;
    property Alignment : TIpHtmlAlign read FAlignment write FAlignment;
    property VAlignment : TIpHtmlVAlign3 read FVAlignment write FVAlignment;
    property LinkColor : TColor read FLinkColor write FLinkColor;
    property VLinkColor : TColor read FVLinkColor write FVLinkColor;
    property ALinkColor : TColor read FALinkColor write FALinkColor;
    property BgColor : TColorRef read FBgColor write FBgColor;
    property Preformatted : Boolean read FPreformatted write FPreformatted;
    property NoBreak : Boolean read FNoBreak write FNoBreak;
    property UseCount : Integer read FUseCount write FUseCount;
    procedure Assign(const Source: TIpHtmlPropB);
    procedure DecUse;
    procedure IncUse;
    constructor CreateCopy(Owner: TIpHtml; Source: TIpHtmlPropB);
    constructor Create(Owner: TIpHtml);
  end;

  TIpHtmlProps = class
  {-class for holding the currently active style attributes}
  protected
    function GetAlignment: TIpHtmlAlign;
    function GetALinkColor: TColor;
    function GetBaseFontSize: integer;
    function GetBgColor: TColorRef;
    function GetFontBaseline: integer;
    function GetFontColor: TColor;
    function GetFontName: string;
    function GetFontSize: integer;
    function GetFontStyle: TFontStyles;
    function GetLinkColor: TColor;
    function GetPreformatted: Boolean;
    function GetVAlignment: TIpHtmlVAlign3;
    function GetVLinkColor: TColor;
    procedure SetAlignment(const Value: TIpHtmlAlign);
    procedure SetALinkColor(const Value: TColor);
    procedure SetBaseFontSize(const Value: integer);
    procedure SetBgColor(const Value: TColorRef);
    procedure SetFontBaseline(const Value: integer);
    procedure SetFontColor(const Value: TColor);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: integer);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetLinkColor(const Value: TColor);
    procedure SetPreformatted(const Value: Boolean);
    procedure SetVAlignment(const Value: TIpHtmlVAlign3);
    procedure SetVLinkColor(const Value: TColor);
    function GetNoBreak: Boolean;
    procedure SetNoBreak(const Value: Boolean);
  protected
    FOwner : TIpHtml;
    PropA : TIpHtmlPropA;
    PropB : TIpHtmlPropB;
  public
    constructor Create(Owner: TIpHtml);
    destructor Destroy; override;
    procedure Assign(Source : TIpHtmlProps);
    function IsEqualTo(Compare: TIpHtmlProps): Boolean;
    function AIsEqualTo(Compare: TIpHtmlProps): Boolean;
    function BIsEqualTo(Compare: TIpHtmlProps): Boolean;
    property BaseFontSize : integer read GetBaseFontSize write SetBaseFontSize;
    property FontName : string read GetFontName write SetFontName;
    property FontSize : integer read GetFontSize write SetFontSize;
    property FontBaseline : integer read GetFontBaseline write SetFontBaseline;
    property FontStyle : TFontStyles read GetFontStyle write SetFontStyle;
    property FontColor : TColor read GetFontColor write SetFontColor;
    property Alignment : TIpHtmlAlign read GetAlignment write SetAlignment;
    property VAlignment : TIpHtmlVAlign3 read GetVAlignment write SetVAlignment;
    property LinkColor : TColor read GetLinkColor write SetLinkColor;
    property VLinkColor : TColor read GetVLinkColor write SetVLinkColor;
    property ALinkColor : TColor read GetALinkColor write SetALinkColor;
    property BgColor : TColorRef read GetBgColor write SetBgColor;
    property Preformatted : Boolean read GetPreformatted write SetPreformatted;
    property NoBreak : Boolean read GetNoBreak write SetNoBreak;
  end;

  TIpHtmlNodeAlignInline = class;

  TElementType = (etWord, etObject, etSoftLF, etHardLF, etClearLeft,
    etClearRight, etClearBoth, etIndent, etOutdent, etSoftHyphen);

  TIpHtmlElement = record
    ElementType : TElementType;
    AnsiWord: string;
    IsBlank : Integer;
    SizeProp: TIpHtmlPropA;
    Size: TSize;
    WordRect2 : TRect;
    Props : TIpHtmlProps;
    Owner : TIpHtmlNode;
    {$IFDEF IP_LAZARUS}
    IsSelected: boolean;
    {$ENDIF}
  end;
  PIpHtmlElement = ^TIpHtmlElement;

  TRectMethod = procedure(const R : TRect) of object;

  TIpHtmlNodeEnumProc = procedure(Node: TIpHtmlNode; const UserData: Pointer) of object;

  {abstract base node}
  TIpHtmlNode = class(TPersistent)
  protected
    FOwner : TIpHtml;
    FParentNode : TIpHtmlNode;
    function PageRectToScreen(const Rect : TRect; var ScreenRect: TRect): Boolean;
    procedure ScreenLine(
      StartPoint, EndPoint : TPoint;
      const Width : Integer;
      const Color : TColor);
    procedure ScreenRect(
      R : TRect;
      const Color : TColor);
    {$IFDEF IP_LAZARUS}
    procedure ScreenFrame(
      R : TRect;
      Raised: boolean);
    {$ENDIF}
    procedure ScreenPolygon(
      Points : array of TPoint;
      const Color : TColor);
    function PagePtToScreen(const Pt: TPoint): TPoint;
    procedure Enqueue; virtual;
    procedure SetProps(const RenderProps: TIpHtmlProps); virtual;
    procedure EnqueueElement(const Entry: PIpHtmlElement); virtual;
    function ElementQueueIsEmpty: Boolean; virtual;                    {!!.10}
    procedure ReportDrawRects(M : TRectMethod); virtual;
    procedure ReportCurDrawRects(Owner: TIpHtmlNode; M : TRectMethod); virtual;
    procedure ReportMapRects(M : TRectMethod); virtual;
    procedure Invalidate; virtual;
    procedure InvalidateSize; virtual;
    procedure SubmitRequest; virtual;
    procedure ResetRequest; virtual;
    function GetHint: string; virtual;
    procedure CreateControl(Parent : TWinControl); virtual;
    procedure MakeVisible; virtual;
    procedure UnmarkControl; virtual;
    procedure HideUnmarkedControl; virtual;
    procedure EnumChildren(EnumProc: TIpHtmlNodeEnumProc; UserData: Pointer); virtual;
    procedure AppendSelection(var S : string); virtual;
    function ExpParentWidth: Integer; virtual;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property Owner : TIpHtml read FOwner;
    procedure ImageChange(NewPicture : TPicture); virtual;
    procedure GetAttributes(Target: TStrings; IncludeValues,
      IncludeBlanks: Boolean);                                         {!!.10}
    procedure SetAttributeValue(const AttrName, NewValue: string);     {!!.10}
  end;

  TIpHtmlNodeNv = class(TIpHtmlNode)
  protected
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;                   {!!.10}
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure Invalidate; override;
    procedure InvalidateSize; override;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  end;

  TIpHtmlNodeMulti = class(TIpHtmlNode)
  protected
    FChildren : TList;
    function GetChildNode(Index: Integer): TIpHtmlNode;
    function GetChildCount: Integer;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure ReportMapRects(M : TRectMethod); override;
    procedure AppendSelection(var S : string); override;
    procedure EnumChildren(EnumProc: TIpHtmlNodeEnumProc; UserData: Pointer); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    property ChildCount : Integer read GetChildCount;
    property ChildNode[Index : Integer] : TIpHtmlNode read GetChildNode;
  end;

  TIpHtmlNodeCore = class(TIpHtmlNodeMulti)
  protected
    FStyle: string;
    FClassId: string;
    FTitle: string;
    FId: string;
    procedure ParseBaseProps(Owner : TIpHtml); {virtual;}              {!!.12}
  published {public}                                                   {!!.10}
    property ClassId : string read FClassId write FClassId;
    property Id : string read FId write FId;
    property Style : string read FStyle write FStyle;
    property Title : string read FTitle write FTitle;
  end;

  TIpHtmlNodeInline = class(TIpHtmlNodeCore)
  protected
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;                   {!!.10}
    procedure Invalidate; override;
  public
  end;

  TIpHtmlImageAlign = (hiaTop, hiaMiddle, hiaBottom, hiaLeft, hiaRight, hiaCenter);
  TIpHtmlNodeAlignInline = class(TIpHtmlNodeInline)
  protected
    FAlignment: TIpHtmlImageAlign;
    Props : TIpHtmlProps;
    Element : PIpHtmlElement;
    procedure Enqueue; override;
    procedure Draw(Block: TIpHtmlNodeBlock); virtual; abstract;
    procedure SetRect(TargetRect: TRect); virtual;
    function GetDim(ParentWidth: Integer): TSize; virtual; abstract;
    procedure CalcMinMaxWidth(var Min, Max: Integer); virtual; abstract;
    procedure SetAlignment(const Value: TIpHtmlImageAlign);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlImageAlign read FAlignment write SetAlignment; {!!.10}
  end;

  TIpHtmlNodeControl = class(TIpHtmlNodeAlignInline)
  protected
    FControl : TWinControl;
    Shown : Boolean;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure HideUnmarkedControl; override;
    procedure UnmarkControl; override;
    procedure AddValues(NameList, ValueList : TStringList); virtual; abstract;
    procedure Reset; virtual; abstract;
    function Successful: Boolean; virtual; abstract;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  end;

  TIpHtmlNodeBlock = class(TIpHtmlNodeCore)
  protected
    FPageRect : TRect;
    ElementQueue : TList;
    FMin, FMax : Integer;
    Props : TIpHtmlProps;
    LastW, LastH : Integer;
    procedure RenderQueue;
    procedure CalcMinMaxQueueWidth(const RenderProps: TIpHtmlProps;
      var Min, Max: Integer);
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;                   {!!.10}
    procedure Render(const RenderProps: TIpHtmlProps); virtual;
    procedure Layout(const RenderProps: TIpHtmlProps;
      const TargetRect : TRect); virtual;
    procedure RelocateQueue(const dx, dy: Integer);
    procedure LayoutQueue(const RenderProps: TIpHtmlProps;
      const TargetRect : TRect);
    procedure CalcMinMaxWidth(const RenderProps: TIpHtmlProps;
      var Min, Max: Integer); virtual;
    procedure ClearWordList;
    procedure Invalidate; override;
    function GetHeight(const RenderProps: TIpHtmlProps;
      const Width: Integer): Integer; {virtual;}                       {!!.12}
    procedure InvalidateSize; override;
    function Level0: Boolean;
    procedure ReportCurDrawRects(Owner: TIpHtmlNode; M : TRectMethod); override;
    property PageRect : TRect read FPageRect;
    procedure AppendSelection(var S : string); override;
    procedure UpdateCurrent(Start: Integer; CurProps : TIpHtmlProps);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  end;

  TIpHtmlDirection = (hdLTR, hdRTL);
  TIpHtmlNodeHEAD = class(TIpHtmlNodeMulti)
  protected
    FProfile: string;
    FLang: string;
    FDir: TIpHtmlDirection;
  published {public}                                                   {!!.10}
    property Dir : TIpHtmlDirection read FDir write FDir;
    property Lang : string read FLang write FLang;
    property Profile : string read FProfile write FProfile;
  end;

  TIpHtmlNodeText = class(TIpHtmlNode)
  protected
    FEscapedText : string;
    PropsR : TIpHtmlProps; {reference}
    function GetAnsiText: string;
    procedure SetAnsiText(const Value: string);
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure BuildWordList;
    procedure SetEscapedText(const Value: string);
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;                   {!!.10}
  published {public}
    property ANSIText : string read GetAnsiText write SetAnsiText;
    property EscapedText : string read FEscapedText write SetEscapedText;
  end;

  TIpHtmlNodeGenInline = class(TIpHtmlNodeInline)
  protected
    Props: TIpHtmlProps;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); virtual; abstract;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  end;

  TIpHtmlNodeFONT = class(TIpHtmlNodeGenInline)
  protected
    FSize: TIpHtmlRelSize;
    FFace: string;
    FColor: TColorRef;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
    procedure SetColor(const Value: TColorRef);
    procedure SetFace(const Value: string);
    procedure SizeChanged(Sender: TObject);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property Color : TColorRef read FColor write SetColor;
    property Face : string read FFace write SetFace;
    property Size : TIpHtmlRelSize read FSize write FSize;
  end;

  TIpHtmlNodeSTYLE = class(TIpHtmlNodeMulti)
  protected
    FMedia: string;
    FTitle: string;
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;                   {!!.10}
  published {public}                                                   {!!.10}
    property Media : string read FMedia write FMedia;
    property Title : string read FTitle write FTitle;
  end;

  TIpHtmlNodeSCRIPT = class(TIpHtmlNodeNv);

  TIpHtmlNodeNOSCRIPT = class(TIpHtmlNodeInline);

  TIpHtmlHeaderSize = 1..6;
  TIpHtmlNodeHeader = class(TIpHtmlNodeInline)
  protected
    FAlign : TIpHtmlAlign;
    FSize : TIpHtmlHeaderSize;
    Props : TIpHtmlProps;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;

  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property Size : TIpHtmlHeaderSize read FSize write FSize;
  end;

  TIpHtmlNodeP = class(TIpHtmlNodeInline)
  protected
    FAlign : TIpHtmlAlign;
    Props : TIpHtmlProps;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure SetAlign(const Value: TIpHtmlAlign);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;

  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write SetAlign;
  end;

  TIpHtmlNodeADDRESS = class(TIpHtmlNodeInline);

  TIpHtmlULType = (ulDisc, ulSquare, ulCircle);
  TIpHtmlNodeList = class(TIpHtmlNodeInline)
  protected
    FCompact : Boolean;
    FListType : TIpHtmlULType;
    procedure Enqueue; override;
    procedure SetListType(const Value: TIpHtmlULType);
  published {public}                                                   {!!.10}
    property Compact : Boolean read FCompact write FCompact;
    property ListType : TIpHtmlULType read FListType write SetListType;
  end;

  TIpHtmlNodeUL = class(TIpHtmlNodeList);
  TIpHtmlNodeDIR = class(TIpHtmlNodeList);
  TIpHtmlNodeMENU = class(TIpHtmlNodeList);

  TIpHtmlOLStyle = (olArabic, olLowerAlpha, olUpperAlpha, olLowerRoman, olUpperRoman);
  TIpHtmlNodeOL = class(TIpHtmlNodeInline)
  protected
    FCompact : Boolean;
    FStart : Integer;
    FStyle : TIpHtmlOLStyle;
    Counter : Integer;
    procedure Enqueue; override;
    function GetNumString : string;
    procedure SetStart(const Value: Integer);
    procedure SetStyle(const Value: TIpHtmlOLStyle);
  published {public}                                                   {!!.10}
    property Compact : Boolean read FCompact write FCompact;
    property Start : Integer read FStart write SetStart;
    property Style : TIpHtmlOLStyle read FStyle write SetStyle;
  end;

  TIpHtmlNodeLI = class(TIpHtmlNodeAlignInline)
  protected
    FCompact: Boolean;
    {FDefListType,}                                                    {!!.12}
    FListType : TIpHtmlULType;
    FValue : Integer;
    WordEntry : PIpHtmlElement;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    function GrossDrawRect: TRect;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    {property DefListType: TIpHtmlULType read FListType write FDefListType;} {!!.12}
    procedure SetListType(const Value: TIpHtmlULType);
    procedure SetValue(const Value: Integer);
  public
    constructor Create(ParentNode : TIpHtmlNode);

  published {public}                                                   {!!.10}
    property Compact : Boolean read FCompact write FCompact;
    property ListType : TIpHtmlULType read FListType write SetListType;
    property Value : Integer read FValue write SetValue;
  end;

  TIpHtmlFormMethod = (hfmGet, hfmPost);
  TIpHtmlNodeFORM = class(TIpHtmlNodeInline)
  protected
    FAccept: string;
    FAcceptCharset: string;
    FName: string;
    FEnctype: string;
    FAction: string;
    FMethod: TIpHtmlFormMethod;
    Props : TIpHtmlProps;
    procedure AddChild(Node: TIpHtmlNode; const UserData: Pointer);
    procedure ResetControl(Node: TIpHtmlNode; const UserData: Pointer);
    procedure ResetForm;
    procedure ResetRequest; override;
    {$IFNDEF HtmlWithoutHttp}
    procedure SubmitForm;
    procedure SubmitRequest; override;
    {$ENDIF}
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;

  published {public}                                                   {!!.10}
    property Accept : string read FAccept write FAccept;
    property AcceptCharset : string read FAcceptCharset write FAcceptCharset;
    property Action : string read FAction write FAction;
    property Enctype : string read FEnctype write FEnctype;
    property Method : TIpHtmlFormMethod read FMethod write FMethod;
    property Name : string read FName write FName;
  end;

  TIpHtmlNodeHtml = class(TIpHtmlNodeMulti)
  protected
    FLang: string;
    FVersion: string;
    FDir: TIpHtmlDirection;
    function HasBodyNode : Boolean;                                    {!!.12}
    procedure Render(const RenderProps: TIpHtmlProps);
    procedure CalcMinMaxWidth(const RenderProps: TIpHtmlProps;
      var Min, Max: Integer);
    function GetHeight(const RenderProps: TIpHtmlProps;
      const Width: Integer): Integer;
    procedure Layout(const RenderProps: TIpHtmlProps;
      const TargetRect : TRect);
  published {public}                                                   {!!.10}
    property Dir : TIpHtmlDirection read FDir write FDir;
    property Lang : string read FLang write FLang;
    property Version : string read FVersion write FVersion;
  end;

  TIpHtmlNodeTITLE = class(TIpHtmlNodeNv)
  protected
    FTitle: string;
  published {public}                                                   {!!.10}
    property Title : string read FTitle write FTitle;
  end;

  TIpHtmlNodeBODY = class(TIpHtmlNodeBlock)
  protected
    FBgColor : TColorRef;
    FText : TColorRef;
    FLink : TColorRef;
    FVLink : TColorRef;
    FALink : TColorRef;
    FBackground : string;
    BGPicture : TPicture;
    procedure Render(const RenderProps: TIpHtmlProps); override;
    procedure SetAlink(const Value: TColorRef);
    procedure SetBackground(const Value: string);
    procedure SetBgcolor(const Value: TColorRef);
    procedure SetLink(const Value: TColorRef);
    procedure SetText(const Value: TColorRef);
    procedure SetVlink(const Value: TColorRef);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure ImageChange(NewPicture : TPicture); override;

  published {public}                                                   {!!.10}
    property ALink : TColorRef read Falink write SetAlink;
    property Background : string read Fbackground write SetBackground;
    property BgColor : TColorRef read Fbgcolor write SetBgcolor;
    property Link : TColorRef read Flink write SetLink;
    property Text : TColorRef read Ftext write SetText;
    property VLink : TColorRef read Fvlink write SetVlink;
  end;

  TIpHtmlNodeNOFRAMES = class(TIpHtmlNodeCore);

  TIpHtmlNodeFRAMESET = class(TIpHtmlNodeCore)
  protected
    FCols: TIpHtmlMultiLengthList;
    FRows: TIpHtmlMultiLengthList;
  public
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property ClassId : string read FClassID write FClassID;
    property Cols : TIpHtmlMultiLengthList read FCols write FCols;
    property Id : string read FId write FId;
    property Rows : TIpHtmlMultiLengthList read FRows write FRows;
    property Style : string read FStyle write FStyle;
    property Title : string read FTitle write FTitle;
  end;

  TIpHtmlFrameScrolling = (hfsAuto, hfsYes, hfsNo);
  TIpHtmlNodeFRAME = class(TIpHtmlNodeCore)
  protected
    FFrameBorder: Integer;
    FLongDesc: string;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    FName: string;
    FNoResize: Boolean;
    FScrolling: TIpHtmlFrameScrolling;
    FSrc: string;
    procedure SetFrameBorder(const Value: Integer);
    procedure SetMarginHeight(const Value: Integer);
    procedure SetMarginWidth(const Value: Integer);
    procedure SetScrolling(const Value: TIpHtmlFrameScrolling);
  published {public}                                                   {!!.10}
    property FrameBorder : Integer read FFrameBorder write SetFrameBorder;
    property LongDesc : string read FLongDesc write FLongDesc;
    property MarginHeight : Integer read FMarginHeight write SetMarginHeight;
    property MarginWidth : Integer read FMarginWidth write SetMarginWidth;
    property Name : string read FName write FName;
    property NoResize : Boolean read FNoResize write FNoResize;
    property Scrolling : TIpHtmlFrameScrolling read FScrolling write SetScrolling;
    property Src : string read FSrc write FSrc;
  end;

  TIpHtmlFrame = class;

  TIpHtmlNodeIFRAME = class(TIpHtmlNodeControl)
  protected
    FAlign: TIpHtmlAlign;
    FFrameBorder: Integer;
    FHeight: TIpHtmlLength;
    FLongDesc: string;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    FName: string;
    FScrolling: TIpHtmlFrameScrolling;
    FSrc: string;
    FWidth: TIpHtmlLength;
    FFrame : TIpHtmlFrame;
    procedure CreateControl(Parent : TWinControl); override;
    function Successful: Boolean; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Reset; override;
    procedure WidthChanged(Sender: TObject);                           {!!.10}
    procedure SetAlign(const Value: TIpHtmlAlign);
    procedure SetFrameBorder(const Value: Integer);
    procedure SetMarginHeight(const Value: Integer);
    procedure SetMarginWidth(const Value: Integer);
    procedure SetScrolling(const Value: TIpHtmlFrameScrolling);
  public
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write SetAlign;
    property Frame: TIpHtmlFrame read FFrame;
    property FrameBorder : Integer read FFrameBorder write SetFrameBorder;
    property Height : TIpHtmlLength read FHeight write FHeight;
    property LongDesc : string read FLongDesc write FLongDesc;
    property MarginHeight : Integer read FMarginHeight write SetMarginHeight;
    property MarginWidth : Integer read FMarginWidth write SetMarginWidth;
    property Name : string read FName write FName;
    property Scrolling : TIpHtmlFrameScrolling read FScrolling write SetScrolling;
    property Src : string read FSrc write FSrc;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeDL = class(TIpHtmlNodeInline)
  protected
    FCompact : Boolean;
    procedure Enqueue; override;                                       {!!.16}
  published {public}                                                   {!!.10}
    property Compact : Boolean read FCompact write FCompact;
  end;

  TIpHtmlNodeDT = class(TIpHtmlNodeInline)
  protected
    procedure Enqueue; override;
  end;

  TIpHtmlNodeDD = class(TIpHtmlNodeInline)
  protected
    procedure Enqueue; override;
  end;

  TIpHtmlNodePRE = class(TIpHtmlNodeInline)
  protected
    Props : TIpHtmlProps;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  end;

  TIpHtmlNodeDIV = class(TIpHtmlNodeInline)
  protected
    FAlign : TIpHtmlAlign;
    Props : TIpHtmlProps;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
  end;

  TIpHtmlNodeSPAN = class(TIpHtmlNodeGenInline)
  protected
    FAlign : TIpHtmlAlign;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
  end;

  TIpHtmlNodeBLINK = class(TIpHtmlNodeInline);

  TIpHtmlNodeBLOCKQUOTE = class(TIpHtmlNodeInline)
  protected
    procedure Enqueue; override;
  end;

  TIpHtmlNodeQ = class(TIpHtmlNodeInline);

  TIpHtmlNodeINS = class(TIpHtmlNodeGenInline)
  protected
    FCite: string;
    FDateTime: string;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  published {public}                                                   {!!.10}
    property Cite : string read FCite write FCite;
    property DateTime : string read FDateTime write FDateTime;
  end;

  TIpHtmlNodeDEL = class(TIpHtmlNodeGenInline)
  protected
    FCite: string;
    FDateTime: string;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  published {public}                                                   {!!.10}
    property Cite : string read FCite write FCite;
    property DateTime : string read FDateTime write FDateTime;
  end;

  TIpHtmlFontStyles = (hfsTT, hfsI, hfsB, hfsU, hfsSTRIKE, hfsS,
   hfsBIG, hfsSMALL, hfsSUB, hfsSUP);
  TIpHtmlNodeFontStyle = class(TIpHtmlNodeGenInline)
  protected
    FStyle : TIpHtmlFontStyles;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  published {public}                                                   {!!.10}
    property Style : TIpHtmlFontStyles read FStyle write FStyle;
  end;

  TIpHtmlPhraseStyle = (hpsEM, hpsSTRONG, hpsDFN, hpsCODE, hpsSAMP,
    hpsKBD, hpsVAR, hpsCITE, hpsABBR, hpsACRONYM);
  TIpHtmlNodePhrase = class(TIpHtmlNodeGenInline)
  protected
    FStyle : TIpHtmlPhraseStyle;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  published {public}                                                   {!!.10}
    property Style : TIpHtmlPhraseStyle read FStyle write FStyle;
  end;

  TIpHtmlNodeHR = class(TIpHtmlNodeAlignInline)
  protected
    FColor: TColorRef;
    FNoShade : Boolean;
    FSize : TIpHtmlInteger;                                            {!!.10}
    FWidth : TIpHtmlLength;
    SizeWidth : TIpHtmlPixels;
    FDim : TSize;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    function GrossDrawRect: TRect;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure Enqueue; override;
    procedure WidthChanged(Sender: TObject);                           {!!.10}
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property Color : TColorRef read FColor write FColor;
    property NoShade  : Boolean read FNoShade write FNoShade;
    property Size : TIpHtmlInteger read FSize write FSize;             {!!.10}
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlBreakClear = (hbcNone, hbcLeft, hbcRight, hbcAll);
  TIpHtmlNodeBR = class(TIpHtmlNodeInline)
  protected
    FClassId: string;
    FStyle: string;
    FTitle: string;
    FId: string;
    FClear: TIpHtmlBreakClear;
    procedure Enqueue; override;
    procedure SetClear(const Value: TIpHtmlBreakClear);
  published {public}                                                   {!!.10}
    property ClassId : string read FClassId write FClassId;
    property Clear : TIpHtmlBreakClear read FClear write SetClear;
    property Id : string read FId write FId;
    property Style : string read FStyle write FStyle;
    property Title : string read FTitle write FTitle;
  end;

  TIpHtmlNodeNOBR = class(TIpHtmlNodeGenInline)
  protected
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  public
  end;

  TIpHtmlMapShape = (hmsDefault, hmsRect, hmsCircle, hmsPoly);
  TIpHtmlNodeA = class(TIpHtmlNodeInline)
  protected
    FTabIndex: Integer;
    FShape: TIpHtmlMapShape;
    FHRef: string;
    FName: string;
    FRel: string;
    FRev: string;
    FTarget: string;
    AreaList : TList;
    MapAreaList : TList;
    Props : TIpHtmlProps;
    FHot: Boolean;
    FHasRef : Boolean;
    {FHasFocus : Boolean;}                                             {!!.12}
    procedure ClearAreaList;
    function PtInRects(const P : TPoint) : Boolean;
    function RelMapPoint(const P: TPoint): TPoint;
    procedure SetHot(const Value: Boolean);
    procedure AddArea(const R: TRect);
    procedure BuildAreaList;
    procedure SetHRef(const Value: string);
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure SetName(const Value: string);
    procedure AddMapArea(const R: TRect);
    function GetHint: string; override;
    procedure DoOnFocus;
    procedure DoOnBlur;
    property HasRef : Boolean read FHasRef;
    property Hot : Boolean read FHot write SetHot;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure MakeVisible; override;

  published {public}                                                   {!!.10}
    property HRef : string read FHRef write SetHRef;
    property Name : string read FName write SetName;
    property Rel : string read FRel write FRel;
    property Rev : string read FRev write FRev;
    property Shape : TIpHtmlMapShape read FShape write FShape;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Target: string read FTarget write FTarget;
  end;

  TIpHtmlNodeIMG = class(TIpHtmlNodeAlignInline)
  protected
    FAlt: string;
    FBorder: Integer;
    FHeight: TIpHtmlPixels{Integer};                                   {!!.10}
    FHSpace: Integer;
    FIsMap: Boolean;
    FLongDesc: string;
    FName: string;
    FPicture : TPicture;
    FSize : TSize;
    FSrc: string;
    FUseMap: string;
    FVSpace: Integer;
    FWidth: TIpHtmlLength;
    NetDrawRect : TRect;
    SizeWidth : TIpHtmlPixels;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure ReportMapRects(M : TRectMethod); override;
    procedure LoadImage;
    procedure UnloadImage;                                             {!!.02}
    function GrossDrawRect: TRect;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure SetUseMap(const Value: string);
    procedure SetBorder(const Value: Integer);
    procedure SetHSpace(const Value: Integer);
    procedure SetVSpace(const Value: Integer);
    function GetHint: string; override;
    procedure DimChanged(Sender: TObject);                             {!!.10}
    procedure InvalidateSize; override;
    {$IFDEF IP_LAZARUS}
    function GetBorder: Integer;
    {$ENDIF}
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure ImageChange(NewPicture : TPicture); override;

  published {public}                                                   {!!.10}
    property Alt : string read FAlt write FAlt;
    {$IFDEF IP_LAZARUS}
    property Border : Integer read GetBorder write SetBorder;
    {$ELSE}
    property Border : Integer read FBorder write SetBorder;
    {$ENDIF}
    property Height : TIpHtmlPixels{Integer} read FHeight write FHeight; {!!.10}
    property HSpace : Integer read FHSpace write SetHSpace;
    property IsMap : Boolean read FIsMap write FIsMap;
    property LongDesc : string read FLongDesc write FLongDesc;
    property Name : string read FName write FName;
    property Picture : TPicture read FPicture;
    property Src : string read FSrc write FSrc;
    property UseMap : string read FUseMap write SetUseMap;
    property VSpace : Integer read FVSpace write SetVSpace;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeAPPLET = class(TIpHtmlNodeInline)
  protected
    FArchive: string;
    FClassID: string;
    FStyle: string;
    FObjectCode: string;
    FTitle: string;
    FId: string;
    FVSpace: Integer;
    FHSpace: Integer;
    FHeight: Integer;
    FWidth: TIpHtmlLength;
    FName: string;
    FCodebase: string;
    FCode: string;
    FAlt: string;
    FAlignment: TIpHtmlImageAlign;
    function GetHint: string; override;
    procedure WidthChanged(Sender: TObject);
  public
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlImageAlign read FAlignment write FAlignment;
    property Archive : string read FArchive write FArchive;
    property Alt : string read FAlt write FAlt;
    property ClassID : string read FClassID write FClassID;
    property Code : string read FCode write FCode;
    property Codebase : string read FCodebase write FCodebase;
    property Height : Integer read FHeight write FHeight;
    property HSpace : Integer read FHSpace write FHSpace;
    property Id : string read FId write FId;
    property Name : string read FName write FName;
    property ObjectCode : string read FObjectCode write FObjectCode;
    property Style : string read FStyle write FStyle;
    property Title : string read FTitle write FTitle;
    property VSpace : Integer read FVSpace write FVSpace;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeOBJECT = class(TIpHtmlNodeInline)
  protected
    FAlignment: TIpHtmlImageAlign;
    FArchive: string;
    FBorder: Integer;
    FClassID: string;
    FCodebase: string;
    FCodeType: string;
    FData: string;
    FDeclare: Boolean;
    FHeight: Integer;
    FHSpace: Integer;
    FName: string;
    FStandby: string;
    FUseMap: string;
    FVSpace: Integer;
    FWidth: TIpHtmlLength;
    procedure WidthChanged(Sender: TObject);                           {!!.10}  
  public
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlImageAlign read FAlignment write FAlignment;
    property Archive : string read FArchive write FArchive;
    property Border : Integer read FBorder write FBorder;
    property ClassID : string read FClassID write FClassID;
    property Codebase : string read FCodebase write FCodebase;
    property CodeType : string read FCodeType write FCodeType;
    property Data : string read FData write FData;
    property Declare : Boolean read FDeclare write FDeclare;
    property Height : Integer read FHeight write FHeight;
    property HSpace : Integer read FHSpace write FHSpace;
    property Name : string read FName write FName;
    property Standby : string read FStandby write FStandby;
    property UseMap : string read FUseMap write FUseMap;
    property VSpace : Integer read FVSpace write FVSpace;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlObjectValueType = (hovtData, hovtRef, hovtObject);
  TIpHtmlNodePARAM = class(TIpHtmlNodeNv)
  protected
    FId: string;
    FValueType: TIpHtmlObjectValueType;
    FValue: string;
    FName: string;
  published {public}                                                   {!!.10}
    property Id : string read FId write FId;
    property Name : string read FName write FName;
    property Value : string read FValue write FValue;
    property ValueType : TIpHtmlObjectValueType read FValueType write FValueType;
  end;

  TIpHtmlNodeBASEFONT = class(TIpHtmlNodeGenInline)
  protected
    FSize: Integer;
    procedure ApplyProps(const RenderProps: TIpHtmlProps); override;
  published {public}                                                   {!!.10}
    property Size : Integer read FSize write FSize;
  end;

  TIpHtmlNodeMAP = class(TIpHtmlNodeCore)
  protected
    FName : string;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;

  published {public}                                                   {!!.10}
    property Name : string read FName write FName;
  end;

  TIpHtmlNodeAREA = class(TIpHtmlNodeCore)
  protected
    FTabIndex: Integer;
    FNoHRef: Boolean;
    FHRef: string;
    FCoords: string;
    FAlt: string;
    FTarget: string;
    FShape: TIpHtmlMapShape;
    FRect : TRect;
    FRgn : HRgn;
    procedure Reset;
    function GetHint: string; override;
    function PtInRects(const P : TPoint) : Boolean;
  public
    destructor Destroy; override;                                      {!!.10}
    {$IFDEF CBuilder}
    property Rect : TRect read FRect;
    {$ENDIF}
    {$IFDEF IP_LAZARUS}
    property Rect : TRect read FRect;
    {$ENDIF}
  published {public}                                                   {!!.10}
    property Alt : string read FAlt write FAlt;
    property Coords : string read FCoords write FCoords;
    property HRef : string read FHRef write FHRef;
    property NoHRef : Boolean read FNoHRef write FNoHRef;
    {$IFNDEF CBuilder}
    {$IFNDEF IP_LAZARUS}
    property Rect : TRect read FRect;
    {$ENDIF}
    {$ENDIF}
    property Shape : TIpHtmlMapShape read FShape write FShape;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Target: string read FTarget write FTarget;
  end;

  TIpHtmlNodeMETA = class(TIpHtmlNodeNv)
  protected
    FScheme: string;
    FContent: string;
    FHttpEquiv: string;
    FName: string;
  published {public}                                                   {!!.10}
    property Content : string read FContent write FContent;
    property HttpEquiv: string read FHttpEquiv write FHttpEquiv;
    property Name : string read FName write FName;
    property Scheme : string read FScheme write FScheme;
  end;

  TIpHtmlNodeLINK = class(TIpHtmlNodeCore)
  protected
    FTitle: string;
    FHRef: string;
    FRev: string;
    FRel: string;
  published {public}                                                   {!!.10}
    property HRef : string read FHRef write FHRef;
    property Rel : string read FRel write FRel;
    property Rev : string read FRev write FRev;
    property Title : string read FTitle write FTitle;
  end;

  TIpHtmlVAlignment2 = (hva2Top, hva2Bottom, hva2Left, hva2Right);

  TIpHtmlNodeCAPTION = class(TIpHtmlNodeBlock)
  protected
    FAlign: TIpHtmlVAlignment2;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlVAlignment2 read FAlign write FAlign;
  end;

  TIpHtmlFrameProp = (hfVoid, hfAbove, hfBelow, hfHSides, hfLhs, hfRhs,
    hfvSides, hfBox, hfBorder);

  TIpHtmlRules = (hrNone, hrGroups, hrRows, hrCols, hrAll);

  {TIntArr = array [0..pred(MAXINTS)] of Integer;}
  TInternalIntArr = array [0..pred(MAXINTS)] of Integer;
  PInternalIntArr = ^TInternalIntArr;
  TIntArr = class
  protected
    InternalIntArr : PInternalIntArr;
    IntArrSize : Integer;
    function GetValue(Index: Integer): Integer;
    procedure SetValue(Index, Value: Integer);
  public
    destructor Destroy; override;
    property Value[Index: Integer]: Integer read GetValue write SetValue; default;
  end;

  TInternalRectArr = array [0..pred(MAXINTS)] of PRect;
  PInternalRectArr = ^TInternalRectArr;
  TRectArr = class
  protected
    InternalRectArr : PInternalRectArr;
    IntArrSize : Integer;
    function GetValue(Index: Integer): PRect;
    procedure SetValue(Index: Integer; Value: PRect);
  public
    destructor Destroy; override;
    property Value[Index: Integer]: PRect read GetValue write SetValue; default;
  end;

  TInternalRectRectArr = array [0..pred(MAXINTS)] of TRectArr;
  PInternalRectRectArr = ^TInternalRectRectArr;
  TRectRectArr = class
  protected
    InternalRectRectArr : PInternalRectRectArr;
    IntArrSize : Integer;
    function GetValue(Index: Integer): TRectArr;
  public
    destructor Destroy; override;
    property Value[Index: Integer]: TRectArr read GetValue; default;
    procedure Delete(Index: Integer);
  end;

  TIpHtmlNodeTABLE = class(TIpHtmlNodeAlignInline)
  protected
    FSummary: string;
    FBgColor: TColorRef;
    FFrame: TIpHtmlFrameProp;
    FRules: TIpHtmlRules;
    FCellSpacing: Integer;
    FBorder: Integer;
    FCellPadding: Integer;
    FWidth: TIpHtmlLength;
    FTableWidth,
    CellOverhead, {sum of col widths + CellOverhead = TableWidth}
    FColCount : Integer;
    ColTextWidth : TIntArr; {actual column widths}
    ColStart : TIntArr; {start of each column relative to table's left}
    ColTextWidthMin,
    ColTextWidthMax : TIntArr; {min and max column widths}
    RowSp : TIntArr; {dynamic flag used for row spanning}
    FCaption : TIpHtmlNodeCAPTION;
    BorderRect : TRect;
    BorderRect2 : TRect; {includes caption if any}
    RUH, RUV : Integer; {ruler width hor/vert}
    BL, BR, BT, BB : Integer; {border width, left, right, top, bottom}
    {$IFNDEF IP_LAZARUS}
    CS2 : Integer; {cell space div 2}
    {$ENDIF}
    SizeWidth : TIpHtmlPixels; {last computed width of table}
    FMin, FMax : Integer;
    procedure CalcMinMaxColTableWidth(const RenderProps: TIpHtmlProps;
      var Min, Max: Integer);
    procedure CalcSize(const ParentWidth: Integer;
       const RenderProps: TIpHtmlProps);
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure SetRect(TargetRect: TRect); override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure InvalidateSize; override;
    function GetColCount: Integer;
    procedure Enqueue; override;
    property ColCount : Integer read GetColCount;
    procedure SetBorder(const Value: Integer);
    procedure SetCellPadding(const Value: Integer);
    procedure SetCellSpacing(const Value: Integer);
    procedure SetFrame(const Value: TIpHtmlFrameProp);
    procedure SetRules(const Value: TIpHtmlRules);
    procedure WidthChanged(Sender: TObject);                           {!!.10}
    function ExpParentWidth: Integer; override;                        {!!.10}
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property BgColor : TColorRef read FBgColor write FBgColor;
    property Border : Integer read FBorder write SetBorder;            {!!.10}
    property CalcMinWidth: Integer read FMin;                          {!!.10}
    property CalcMaxWidth: Integer read FMax;                          {!!.10}
    property CalcTableWidth: Integer read FTableWidth;                 {!!.10}
    property CellPadding : Integer
                         read FCellPadding write SetCellPadding;       {!!.10}
    property CellSpacing : Integer
                         read FCellSpacing write SetCellSpacing;       {!!.10}
    property Frame : TIpHtmlFrameProp read FFrame write SetFrame;      {!!.10}
    property Rules : TIpHtmlRules read FRules write SetRules;          {!!.10}
    property Summary : string read FSummary write FSummary;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeTHeadFootBody = class(TIpHtmlNodeCore);

  TIpHtmlNodeTABLEHEADFOOTBODYClass = class of TIpHtmlNodeTHeadFootBody;

  TIpHtmlNodeTHEAD = class(TIpHtmlNodeTHeadFootBody)
  protected
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
  public
    constructor Create(ParentNode : TIpHtmlNode);
  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
  end;

  TIpHtmlNodeTFOOT = class(TIpHtmlNodeTHeadFootBody)
  protected
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
  end;

  TIpHtmlNodeTBODY = class(TIpHtmlNodeTHeadFootBody)
  protected
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
  public
    constructor Create(ParentNode : TIpHtmlNode);

  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
  end;

  TIpHtmlNodeCOLGROUP = class(TIpHtmlNodeCore)
  protected
    FAlign: TIpHtmlAlign;
    FSpan: Integer;
    FVAlign: TIpHtmlVAlign3;
    FWidth: TIpHtmlMultiLength;
  public
    destructor Destroy; override;                                      {!!.10}
  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property Span : Integer read FSpan write FSpan;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
    property Width : TIpHtmlMultiLength read FWidth write FWidth;
  end;

  TIpHtmlNodeCOL = class(TIpHtmlNodeCore)
  protected
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
    FSpan: Integer;
    FWidth: TIpHtmlMultiLength;
  public
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property Span : Integer read FSpan write FSpan;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
    property Width : TIpHtmlMultiLength read FWidth write FWidth;
  end;

  TIpHtmlNodeTR = class(TIpHtmlNodeCore)
  protected
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign;
  public
    constructor Create(ParentNode : TIpHtmlNode);

  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property VAlign : TIpHtmlVAlign read FVAlign write FVAlign;
  end;

  TIpHtmlCellScope = (hcsUnspec, hcsRow, hcsCol, hcsRowGroup, hcsColGroup);

  TIpHtmlNodeTableHeaderOrCell = class(TIpHtmlNodeBlock)
  protected
    FCalcWidthMin: Integer;                                            {!!.10}
    FCalcWidthMax: Integer;                                            {!!.10}
    FNowrap: Boolean;
    FHeight: TIpHtmlPixels{Integer};                                   {!!.10}
    FRowspan: Integer;
    FWidth: TIpHtmlLength;
    FColspan: Integer;
    FAlign: TIpHtmlAlign;
    FVAlign: TIpHtmlVAlign3;
    FBgColor : TColorRef;
    FPadRect : TRect;
    procedure Render( const RenderProps: TIpHtmlProps); override;
    procedure Layout(const RenderProps: TIpHtmlProps; const TargetRect : TRect); override;
    procedure CalcMinMaxWidth(const RenderProps: TIpHtmlProps;
      var Min, Max: Integer); override;
    property PadRect : TRect read FPadRect;
    procedure DimChanged(Sender: TObject);                             {!!.10}
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlAlign read FAlign write FAlign;
    property BgColor : TColorRef read FBgColor write FBgColor;
    property CalcWidthMin: Integer read FCalcWidthMin;                 {!!.10}
    property CalcWidthMax: Integer read FCalcWidthMax;                 {!!.10}
    property Colspan : Integer read FColspan write FColspan;
    property Height : TIpHtmlPixels{Integer} read FHeight write FHeight; {!!.10}
    property Nowrap : Boolean read FNowrap write FNowrap;
    property Rowspan : Integer read FRowspan write FRowspan;
    property VAlign : TIpHtmlVAlign3 read FVAlign write FVAlign;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TIpHtmlNodeTH = class(TIpHtmlNodeTableHeaderOrCell);

  TIpHtmlNodeTD = class(TIpHtmlNodeTableHeaderOrCell);

  TIpHtmlInputType = (hitText, hitPassword, hitCheckbox, hitRadio,
    hitSubmit, hitReset, hitFile, hitHidden, hitImage, hitButton);

  TIpHtmlNodeINPUT = class(TIpHtmlNodeControl)
  protected
    FReadOnly: Boolean;
    FDisabled: Boolean;
    FTabIndex: Integer;
    FAlt: string;
    FChecked: Boolean;
    FMaxLength: Integer;
    FSize: Integer;
    FSrc: string;
    FValue: string;
    FName: string;
    FInputType: TIpHtmlInputType;
    FPicture : TPicture;
    FFileEdit : TEdit;
    FFileSelect : TButton;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    procedure SubmitClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure FileSelect(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    function GetHint: string; override;
    procedure SetImageGlyph(Picture: TPicture);
    procedure CreateControl(Parent : TWinControl); override;
    function Successful: Boolean; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Reset; override;
  public
    destructor Destroy; override;
    procedure ImageChange(NewPicture : TPicture); override;

  published {public}                                                   {!!.10}
    property Alt : string read FAlt write FAlt;
    property Checked : Boolean read FChecked write FChecked;
    property Disabled : Boolean read FDisabled write FDisabled;
    property InputType : TIpHtmlInputType read FInputType write FInputType;
    property MaxLength : Integer read FMaxLength write FMaxLength;
    property Name : string read FName write FName;
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    property Size : Integer read FSize write FSize;
    property Src : string read FSrc write FSrc;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Value : string read FValue write FValue;
  end;

  TIpHtmlButtonType = (hbtSubmit, hbtReset, hbtButton);

  TIpHtmlNodeBUTTON = class(TIpHtmlNodeControl)
  protected
    FDisabled: Boolean;
    FTabIndex: Integer;
    FValue: string;
    FName: string;
    FInputType: TIpHtmlButtonType;
    procedure SubmitClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    function Successful: Boolean; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure CreateControl(Parent : TWinControl); override;
    procedure Reset; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;

  published {public}                                                   {!!.10}
    property ButtonType : TIpHtmlButtonType read FInputType write FInputType;
    property Disabled : Boolean read FDisabled write FDisabled;
    property Name : string read FName write FName;
    property TabIndex : Integer read FTabIndex write FTabIndex;
    property Value : string read FValue write FValue;
  end;

  TIpHtmlNodeSELECT = class(TIpHtmlNodeControl)
  protected
    FDisabled: Boolean;
    FMultiple: Boolean;
    FName: string;
    FSize: Integer;
    FTabIndex: Integer;
    procedure CreateControl(Parent : TWinControl); override;
    function Successful: Boolean; override;
    procedure Reset; override;
    procedure ButtonClick(Sender: TObject);                            {!!.01}
  public
    procedure AddValues(NameList, ValueList : TStringList); override;

  published {public}                                                   {!!.10}
    property Disabled : Boolean read FDisabled write FDisabled;
    property Multiple : Boolean read FMultiple write FMultiple;
    property Name : string read FName write FName;
    property Size : Integer read FSize write FSize;
    property TabIndex : Integer read FTabIndex write FTabIndex;
  end;

  TIpHtmlNodeOPTION = class(TIpHtmlNodeCore)
  protected
    FDisabled: Boolean;
    FOptionLabel: string;
    FSelected: Boolean;
    FValue: string;
  published {public}                                                   {!!.10}
    property Disabled : Boolean read FDisabled write FDisabled;
    property OptionLabel : string read FOptionLabel write FOptionLabel;
    property Selected : Boolean read FSelected write FSelected;
    property Value : string read FValue write FValue;
  end;

  TIpHtmlNodeOPTGROUP = class(TIpHtmlNodeCore)
  protected
    FDisabled: Boolean;
    FGroupLabel: string;
  published {public}                                                   {!!.10}
    property Disabled : Boolean read FDisabled write FDisabled;
    property GroupLabel : string read FGroupLabel write FGroupLabel;
  end;

  TIpHtmlNodeTEXTAREA = class(TIpHtmlNodeControl)
  protected
    FDisabled: Boolean;
    FReadOnly: Boolean;
    FTabIndex: Integer;
    FCols: Integer;
    FRows: Integer;
    FName: string;
    procedure CreateControl(Parent : TWinControl); override;
    function Successful: Boolean; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Reset; override;
  published {public}                                                   {!!.10}
    property Cols : Integer read FCols write FCols;
    property Disabled : Boolean read FDisabled write FDisabled;
    property Name : string read FName write FName;
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    property Rows : Integer read FRows write FRows;
    property TabIndex : Integer read FTabIndex write FTabIndex;
  end;

  TInvalidateEvent = procedure(Sender : TIpHtml; const Rect : TRect) of object;

  TIpHtmlNodeLABEL = class(TIpHtmlNodeInline)
  protected
    FLabelFor: string;
  public
    constructor Create(ParentNode: TIpHtmlNode);
    destructor Destroy; override;
  published {public}                                                   {!!.10}
    property LabelFor : string read FLabelFor write FLabelFor;
  end;

  TIpHtmlNodeFIELDSET = class(TIpHtmlNodeCore);

  TIpHtmlNodeLEGEND = class(TIpHtmlNodeCore)
  protected
    FAlign: TIpHtmlVAlignment2;
  published {public}                                                   {!!.10}
    property Align : TIpHtmlVAlignment2 read FAlign write FAlign;
  end;

  TWriteCharProvider = procedure(C : AnsiChar) of object;

  TIpHtmlDataGetImageEvent =
    procedure(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture)
      of object;

  TIpHtmlScrollEvent =
    procedure(Sender: TIpHtml; const R: TRect) of object;

  TGetEvent =
    procedure(Sender: TIpHtml; const URL: string) of object;

  TPostEvent =
    procedure(Sender: TIpHtml; const URL: string;
      FormData: TIpFormDataEntity) of object;     {!!.12}

  TIFrameCreateEvent =
    procedure(Sender: TIpHtml; Parent: TWinControl;
      Frame: TIpHtmlNodeIFRAME;
      var Control: TWinControl) of object;

  TURLCheckEvent =
    procedure(Sender: TIpHtml; const URL: string;
      var Visited: Boolean) of object;

  TReportURLEvent =
    procedure(Sender: TIpHtml; const URL: string) of object;

  TIpHtmlRectListEntry = record
    Rect : TRect;
    Node : PIpHtmlElement;
    Block : TIpHtmlNodeBlock;
  end;
  PIpHtmlRectListEntry = ^TIpHtmlRectListEntry;

  TControlEvent = procedure(Sender: TIpHtml; Node: TIpHtmlNodeControl)
    of object;

  TIpHtml = class
  protected
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    CharStream : TStream;
    CurToken : TIpHtmlToken;
    ParmList, ValueList : TStringList;
    FHtml : TIpHtmlNodeHtml;
    CharStack : array [0..7] of AnsiChar;
    LastWasSpace: Boolean;                                             {!!.10}
    LastWasClose: Boolean;                                             {!!.10}
    CharSP : integer;
    FFlagErrors : Boolean;
    IndexPhrase : string;
    {Base : string;}                                                   {!!.12}
    {IsIndexPresent : Boolean;}                                        {!!.12}
    FHasFrames : Boolean;
    TokenBuffer : TIpHtmlToken;
    FPageRect : TRect;
    HaveToken : Boolean;
    PageViewRect : TRect; {the current section of the page}
    ClientRect : TRect;   {the coordinates of the paint rectangle}
    FHotNode : TIpHtmlNode;
    FCurElement : PIpHtmlElement;
    FHotPoint : TPoint;
    FOnInvalidateRect : TInvalidateEvent;
    FControlClick : TControlEvent;
    FControlCreate : TControlEvent;
    FTarget : TCanvas;
    DefaultProps : TIpHtmlProps;
    Body : TIpHtmlNodeBODY;
    FVLinkColor: TColorRef;
    FLinkColor: TColorRef;
    FAlinkColor: TColorRef;
    FTextColor: TColorRef;
    FTitleNode : TIpHtmlNodeTITLE;
    {$IFDEF UseGifImageUnit}
    GifImages : TList;
    {$ELSE}
    AnimationFrames : TList;
    {$ENDIF}
    FOnGetImageX : TIpHtmlDataGetImageEvent;
    FOnScroll : TIpHtmlScrollEvent;
    CurFrameSet : TIpHtmlNodeFRAMESET;
    LIndent, LOutdent : PIpHtmlElement;
    SoftLF,
    HardLF, HardLFClearLeft, SoftHyphen,
    HardLFClearRight, HardLFClearBoth : PIpHtmlElement;
    NameList : TStringList;
    FOnInvalidateSize : TNotifyEvent;
    {PanelWidth : Integer;}                                            {!!.12}
    GifQueue : TList;
    InPre : Integer;
    InBlock : Integer;
    MapList : TList;
    AreaList : TList;
    DefaultImage : TPicture;
    MapImgList : TList;
    FOnGet: TGetEvent;
    FOnPost: TPostEvent;
    GlobalPos, LineNumber, LineOffset : Integer;
    PaintBufferBitmap : TBitmap;
    PaintBuffer : TCanvas;
    TokenStringBuf : PChar; {array[16383] of AnsiChar;}        {!!.01}
    TBW : Integer;
    Destroying : Boolean;
    AllSelected : Boolean;
    HtmlTokenList : TStringList;
    FCanPaint : Boolean;
    RectList : TList;
    FStartSel, FEndSel : TPoint;
    FOnIFrameCreate : TIFrameCreateEvent;
    ElementPool : TIpHtmlPoolManager;
    FOnURLCheck: TURLCheckEvent;
    FOnReportURL: TReportURLEvent;

    AnchorList : TList;
    ControlList : TList;
    CURURL : string;
    DoneLoading : Boolean;
    ListLevel : Integer;

    PropACache : TList;
    PropBCache : TList;
    DummyA : TIpHtmlPropA;
    DummyB : TIpHtmlPropB;

    RenderCanvas : TCanvas;
    PageHeight : Integer;
    StartPos : Integer;
    FFixedTypeface: string;                                            {!!.10}
    {$IFDEF IP_LAZARUS}
    FDefaultTypeFace: string;
    {$ENDIF}
    ParmBuf: PChar;                                                    {!!.12}
    ParmBufSize: Integer;                                              {!!.12}
    procedure ResetCanvasData;
    procedure ResetCache;
    procedure ResetWordLists;
    procedure ResetBlocks(Node: TIpHtmlNode);
    procedure ResetImages(Node: TIpHtmlNode);                          {!!.02}
    procedure ResetElementMetrics(P: Pointer);
    function FindPropA(const pFontName: string; const pFontSize: Integer;
      const pFontStyle: TFontStyles;
      const pBaseFontSize: Integer): TIpHtmlPropA;
    function FindPropB(const pFontBaseline: integer;
      const pFontColor: TColor; const pAlignment: TIpHtmlAlign;
      const pVAlignment: TIpHtmlVAlign3; const pLinkColor, pVLinkColor,
      pALinkColor: TColor; const pBgColor: TColorRef; const pPreformatted,
      pNoBreak: Boolean): TIpHtmlPropB;
    procedure ClearCache;
    function CheckKnownURL(URL: string): boolean;
    procedure ReportReference(URL: string);
    procedure PaintSelection;
    function PageRectToScreen(const Rect: TRect;
      var ScreenRect: TRect): Boolean;
    function IsWhiteSpace: Boolean;
    function GetTokenString: string;
    procedure ReportError(const ErrorMsg: string);
    procedure ReportExpectedError(const ErrorMsg: string);
    procedure ReportExpectedToken(const Token: TIpHtmlToken);
    procedure EnsureClosure(const EndToken: TIpHtmlToken;
      const EndTokens: TIpHtmlTokenSet);
    function NewElement(EType : TElementType; Own: TIpHtmlNode) : PIpHtmlElement;
    function BuildStandardEntry(EType: TElementType): PIpHtmlElement;
    function ParseDir: TIpHtmlDirection;
    procedure ParseSPAN(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseQ(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseINS(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseDEL(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseTableBody(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseTableRows(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseColGroup(Parent: TIpHtmlNode);
    function ParseFrameScrollingProp: TIpHtmlFrameScrolling;
    function ParseObjectValueType: TIpHtmlObjectValueType;
    procedure ParseFrameSet(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseFrame(Parent : TIpHtmlNode);
    procedure ParseIFrame(Parent : TIpHtmlNode);
    procedure ParseNOFRAMES(Parent : TIpHtmlNode);
    function ParseButtonType: TIpHtmlButtonType;
    procedure ParseNoscript(Parent: TIpHtmlNode);
    procedure ParseLEFT(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBLINK(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseRIGHT(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure PutToken(Token: TIpHtmlToken);
    procedure ParseParagraph(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseListItems(Parent : TIpHtmlNodeCore;
        EndToken: TIpHtmlToken; const EndTokens : TIpHtmlTokenSet;
        DefaultListStyle : TIpHtmlULType);
    procedure ParseUnorderedList(Parent: TIpHtmlNode;
          EndToken : TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
    procedure ParseOrderedList(Parent: TIpHtmlNode;
        const EndTokens : TIpHtmlTokenSet);
    procedure ParseDefinitionList(Parent: TIpHtmlNode;
                const EndTokens: TIpHtmlTokenSet);
    procedure ParseDefListItems(Parent: TIpHtmlNode;
        const EndTokens: TIpHtmlTokenSet);
    procedure ParsePre(ParentNode : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseDIV(Parent : TIpHtmlNode;
                const EndTokens: TIpHtmlTokenSet);
    procedure ParseCENTER(Parent: TIpHtmlNode;
          const EndTokens: TIpHtmlTokenSet);
    procedure ParseBLOCKQUOTE(Parent : TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseHR(Parent: TIpHtmlNode);
    procedure ParseFontStyle(Parent: TIpHtmlNode;
      StartToken : TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
    procedure ParsePhraseElement(Parent: TIpHtmlNode;
      StartToken, EndToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
    procedure ParseAnchor(Parent: TIpHtmlNode;
        const EndTokens : TIpHtmlTokenSet);
    procedure ParseIMG(Parent : TIpHtmlNode);
    procedure ParseApplet(Parent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
    procedure ParseOBJECT(Parent : TIpHtmlNode);
    procedure ParseBasefont(Parent: TIpHtmlNode);
    procedure ParseBR(Parent : TIpHtmlNode);
    procedure ParseNOBR(Parent: TIpHtmlNode);
    procedure ParseMAP(Parent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseTABLE(Parent: TIpHtmlNode;
        const EndTokens: TIpHtmlTokenSet);
    function FindAttribute(const AttrName: string): string;
    function ColorFromString(S: string): TColorRef;
    function ParseAlignment: TIpHtmlAlign;
    function ParseCellAlign(Default : TIpHtmlAlign) : TIpHtmlAlign;
    function ParseFrameProp(Default: TIpHtmlFrameProp) : TIpHtmlFrameProp;
    function ParseRules(Default : TIpHtmlRules) : TIpHtmlRules;
    function ParseULStyle(Default : TIpHtmlULType): TIpHtmlULType;
    function ParseBoolean(const AttrName: string): Boolean;
    function ParseInteger(const AttrName: string;
      Default: Integer): Integer;
    function ParseHtmlInteger(const AttrName: string;
      Default: Integer): TIpHtmlInteger;                               {!!.10}
    function ParsePixels(const AttrName, Default: string): TIpHtmlPixels; {!!.10}
    function ParseHyperLength(const AttrName: string;
      const Default: string): TIpHtmlLength;
    function ParseHyperMultiLength(const AttrName: string;
      const Default: string): TIpHtmlMultiLength;
    function ParseHyperMultiLengthList(const AttrName: string;
      const Default: string): TIpHtmlMultiLengthList;                  {!!.10}
    function ParseOLStyle(Default: TIpHtmlOLStyle): TIpHtmlOLStyle;
    function ParseImageAlignment(const Default: string): TIpHtmlImageAlign;
    function ParseVAlignment : TIpHtmlVAlign;
    function ParseVAlignment2 : TIpHtmlVAlignment2;
    function ParseVAlignment3 : TIpHtmlVAlign3;
    function ParseRelSize{(const Default: string)}: TIpHtmlRelSize;    {!!.10}
    function ParseBRClear: TIpHtmlBreakClear;
    function ParseShape: TIpHtmlMapShape;
    function NextChar : AnsiChar;
    procedure Parse;
    {procedure ParseDocType;}
    procedure ParseHtml;
    function GetChar: AnsiChar;
    procedure NextToken;
    procedure PutChar(Ch: AnsiChar);
    procedure ParseHead(Parent : TIpHtmlNode);
    procedure ParseHeadItems(Parent : TIpHtmlNode);
    procedure ParseTitle(Parent: TIpHtmlNode);
    procedure ParseScript(Parent : TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
    procedure ParseStyle(ParentNode : TIpHtmlNode);
    procedure ParseIsIndex;
    procedure ParseBase;
    procedure ParseLink(Parent : TIpHtmlNode);
    procedure ParseMeta(Parent : TIpHtmlNode);
    procedure ParseBody(Parent : TIpHtmlNode;
          const EndTokens: TIpHtmlTokenSet);
    procedure ParseBodyText(Parent : TIpHtmlNode;
      const EndTokens: TIpHtmlTokenSet);
    procedure ParseBlock(Parent: TIpHtmlNode;
      const EndTokens: TIpHtmlTokenSet);
    procedure ParseInline(Parent: TIpHtmlNode;
      const EndTokens: TIpHtmlTokenSet);
    procedure ParseHeader(Parent : TIpHtmlNode;
      EndToken : TIpHtmlToken; Size : Integer);
    procedure ParseText(const EndTokens: TIpHtmlTokenSet;
         Parent: TIpHtmlNode);
    procedure ParseFont(Parent : TIpHtmlNode;
       const EndTokens: TIpHtmlTokenSet);
    procedure ParseAddress(Parent: TIpHtmlNode);
    procedure ParseForm(Parent: TIpHtmlNode;
        const EndTokens: TIpHtmlTokenSet);
    function ParseMethod: TIpHtmlFormMethod;
    procedure ParseTableRow(Parent: TIpHtmlNode;
        const EndTokens : TIpHtmlTokenSet);
    function ParseInputType : TIpHtmlInputType;
    procedure ParseFormFields(Parent: TIpHtmlNode;
          const EndTokens : TIpHtmlTokenSet);
    procedure InvalidateRect(R : TRect);
    procedure SetDefaultProps;
    function BuildPath(const Ext: string): string;
    procedure MakeVisible(const R: TRect);
    procedure InvalidateSize;
    procedure AddGifQueue(Graphic: TGraphic; const R: TRect);
    procedure ClearGifQueue;
    procedure StartGifPaint(Target: TCanvas);
    procedure ClearAreaLists;
    function PagePtToScreen(const Pt: TPoint): TPoint;
    procedure NextRealToken;
    procedure SkipTextTokens;
    procedure BuildAreaList;
    procedure ClearAreaList;
    procedure NextNonBlankToken;
    procedure Get(const URL: string);
    procedure Post(const URL: string; FormData: TIpFormDataEntity); {!!.12}
    procedure ClearRectList;
    procedure AddRect(const R: TRect; Node: PIpHtmlElement;
      Block: TIpHtmlNodeBlock);
    procedure CreateIFrame(Parent: TWinControl; Frame: TIpHtmlNodeIFRAME;
      var Control: TWinControl);
    procedure FinalizeRecs(P: Pointer);
    function LinkVisited(const URL: string): Boolean;
    procedure AddWord(Value: string; Props: TIpHtmlProps;
      Owner: TIpHtmlNode);
    procedure AddWordEntry(const Value: string; Props: TIpHtmlProps;
      Owner: TIpHtmlNode);
    function FindElement(const Name: string): TIpHtmlNode;
    procedure Clear; {clear any contents}
    procedure Home;
    function GetPageRect(TargetCanvas: TCanvas; Width, Height : Integer): TRect;
    procedure MouseMove(Pt : TPoint);
    {$IFDEF IP_LAZARUS}
    procedure DeselectAllItems(Item: Pointer);
    {$ENDIF}
    procedure SetSelection(StartPoint, EndPoint: TPoint);
    function HaveSelection: Boolean;
    procedure CopyToClipboard;
    procedure ReportReferences(Node: TIpHtmlNode);
    procedure RequestImageNodes(Node: TIpHtmlNode);
    procedure SelectAll;
    procedure DeselectAll;
    procedure ControlClick(Sender: TIpHtmlNodeControl);
    procedure ControlCreate(Sender: TIpHtmlNodeControl);
    property HotNode : TIpHtmlNode read FHotNode;
    property CurElement : PIpHtmlElement read FCurElement write FCurElement;
    property HotPoint : TPoint read FHotPoint;
    property OnInvalidateRect : TInvalidateEvent
      read FOnInvalidateRect write FOnInvalidateRect;
    property Target : TCanvas read FTarget;
    property TextColor : TColorRef read FTextColor write FTextColor;
    property LinkColor : TColorRef read FLinkColor write FLinkColor;
    property VLinkColor : TColorRef read FVLinkColor write FVLinkColor;
    property ALinkColor : TColorRef read FAlinkColor write FAlinkColor;
    property HasFrames : Boolean read FHasFrames;
    property OnGetImageX : TIpHtmlDataGetImageEvent
                read FOnGetImageX write FOnGetImageX;
    property OnScroll : TIpHtmlScrollEvent
                read FOnScroll write FOnScroll;
    property OnInvalidateSize : TNotifyEvent
                read FOnInvalidateSize write FOnInvalidateSize;
    property OnGet : TGetEvent
                read FOnGet write FOnGet;
    property OnPost : TPostEvent
                read FOnPost write FOnPost;
    property OnIFrameCreate : TIFrameCreateEvent
                read FOnIFrameCreate write FOnIFrameCreate;
    property OnURLCheck : TURLCheckEvent
                read FOnURLCheck write FOnURLCheck;
    property OnReportURL: TReportURLEvent
                read FOnReportURL write FOnReportURL;
    property OnControlClick : TControlEvent
                read FControlClick write FControlClick;
    property OnControlCreate : TControlEvent
                read FControlCreate write FControlCreate;
    property FrameSet : TIpHtmlNodeFRAMESET read CurFrameSet;
    property CanPaint : Boolean read FCanPaint;
    property MarginWidth : Integer
                read FMarginWidth write FMarginWidth default 20;
    property MarginHeight : Integer
                read FMarginHeight write FMarginHeight default 20;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    {$IFOPT C+}
    procedure CheckImage(Picture: TPicture);
    {$ENDIF}
    {$IFDEF IP_LAZARUS}
    function GetSelectionBlocks(out StartSelIndex,EndSelIndex: integer): boolean;
    {$ENDIF}
    
  public
    constructor Create;
    destructor Destroy; override;
    property FlagErrors : Boolean read FFlagErrors write FFlagErrors;
    property FixedTypeface: string read FFixedTypeface write FFixedTypeface;
    {$IFDEF IP_LAZARUS}
    property DefaultTypeFace: string read FDefaultTypeFace write FDefaultTypeFace;
    {$ENDIF}
    property HtmlNode : TIpHtmlNodeHtml read FHtml;
    procedure LoadFromStream(S : TStream);
    procedure Render(TargetCanvas: TCanvas; TargetPageRect : TRect;
      UsePaintBuffer: Boolean; const TopLeft: TPoint);                 {!!.10}
    property TitleNode : TIpHtmlNodeTITLE read FTitleNode;
    {$IFDEF IP_LAZARUS_DBG}
    procedure DebugChild(Node: TIpHtmlNode; const UserData: Pointer);
    procedure DebugAll;
    {$ENDIF}
  end;

  TIpHtmlFocusRect = class(TCustomControl)
  protected
    FAnchor : TIpHtmlNodeA;
    {HaveFocus : Boolean;}                                             {!!.12}
    procedure CreateParams(var Params: TCreateParams); override;
    {$IFDEF IP_LAZARUS}
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    {$ELSE}
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    property Anchor : TIpHtmlNodeA read FAnchor write FAnchor;
  end;

  TIpHtmlInternalPanel = class;

  TIpHtmlScrollBar = class
  protected
    FControl: TIpHtmlInternalPanel;
    FIncrement: TScrollBarInc;
    FPageIncrement: TScrollbarInc;
    FPosition: Integer;
    FRange: Integer;
    FCalcRange: Integer;
    FKind: TScrollBarKind;
    FVisible: Boolean;
    FTracking: Boolean;
    {FDelay: Integer;}                                                 {!!.12}
    {FColor: TColor;}                                                  {!!.12}
    {FParentColor: Boolean;}                                           {!!.12}
    {FPageDiv: Integer;}                                               {!!.12}
    {FLineDiv: Integer;}                                               {!!.12}
    FUpdateNeeded: Boolean;
    {$IFNDEF IP_LAZARUS}
    constructor Create(AControl: TIpHtmlInternalPanel; AKind: TScrollBarKind);
    {$ENDIF}
    procedure CalcAutoRange;
    function ControlSize(ControlSB, AssumeSB: Boolean): Integer;
    procedure DoSetRange(Value: Integer);
    function NeedsScrollBarVisible: Boolean;
    procedure ScrollMessage(var Msg: TWMScroll);
    procedure SetPosition(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure Update(ControlSB, AssumeSB: Boolean);
  public
    {$IFDEF IP_LAZARUS}
    constructor Create(AControl: TIpHtmlInternalPanel; AKind: TScrollBarKind);
    {$ENDIF}
    property Kind: TScrollBarKind read FKind;
    property Increment: TScrollBarInc
                read FIncrement write FIncrement stored False default 8;
    property Position: Integer read FPosition write SetPosition default 0;
    property Range: Integer
                read FRange {write SetRange stored IsRangeStored default 0};
    property Tracking: Boolean read FTracking write FTracking default False;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TIpHtmlCustomPanel = class;

  { TIpHtmlInternalPanel }

  TIpHtmlInternalPanel = class(
    {$IFDEF IP_LAZARUS}TCustomControl{$ELSE}TCustomPanel{$ENDIF})
  protected
    FUpdatingScrollbars : Boolean;
    FAutoScroll: Boolean;
    FPageRect : TRect;
    InPrint: Integer;                                                  {!!.10}
    FOnHotChange : TNotifyEvent;
    FOnCurElementChange : TNotifyEvent;
    FOnHotClick : TNotifyEvent;
    FOnClick : TNotifyEvent;
    FHyper : TIpHtml;
    SettingPageRect : Boolean;
    MouseDownX, MouseDownY : Integer;
    HaveSelection,
    MouseIsDown,
    NewSelection : Boolean;
    SelStart, SelEnd : TPoint;
    HintWindow : THintWindow;
    CurHint : string;
    HintX, HintY : Integer;
    HintShownHere : Boolean;
    Printed: Boolean; {!!.10}
    procedure UpdateScrollBars;
    procedure SetPageRect(const Value: TRect);
    procedure SetHtml(const Value: TIpHtml);
    procedure ClearSelection;
    procedure SetSelection;
    procedure ScrollPtInView(P: TPoint);
    procedure ShowHintNow(const NewHint: string);                      {!!.12}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    {$IFDEF IP_LAZARUS}
    procedure AsyncHotInvoke(data: ptrint);
    {$ENDIF}

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DoHotChange;
    procedure DoCurElementChange;
    procedure DoHotInvoke;
    procedure DoClick;
    procedure Resize; override;
    procedure ScrollInView(R : TRect);
    procedure ScrollInViewRaw(R : TRect);
    function PagePtToScreen(const Pt : TPoint): TPoint;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure DoOnMouseWheel(Shift : TShiftState; Delta, XPos, YPos : SmallInt);
    procedure HideHint;
    function HtmlPanel: TIpHtmlCustomPanel;
    procedure BeginPrint;                                              {!!.10}
    procedure EndPrint;                                                {!!.10}
  public
    ViewTop, ViewLeft : Integer;
    HScroll,
    VScroll : TIpHtmlScrollBar;
    PrintPageRect : TRect;
    PrintWidth, PrintHeight: Integer;                                  {!!.10}
    PrintTopLeft: TPoint;                                              {!!.10}
    {PrintBottomRight: TPoint;}                                 {!!.10}{!!.12}
    PageCount: Integer;                                                {!!.10}
    procedure InvalidateSize;
    property Hyper : TIpHtml read FHyper write SetHtml;
    property PageRect : TRect read FPageRect write SetPageRect;
    constructor Create(AOwner: TComponent); override;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
    property OnHotChange : TNotifyEvent read FOnHotChange write FOnHotChange;
    property OnCurElementChange: TNotifyEvent
                read FOnCurElementChange write FOnCurElementChange;
    property OnHotClick : TNotifyEvent read FOnHotClick write FOnHotClick;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
    destructor Destroy; override;
    procedure ScrollRequest(Sender: TIpHtml; const R: TRect);
    {$IFDEF Version4}
    procedure MouseWheelHandler(var Message: TMessage); override;
    {$ENDIF}
    function GetPrintPageCount: Integer;
    procedure PrintPages(FromPage, ToPage: Integer);
    procedure PrintPreview;
    procedure EraseBackground(DC: HDC); override;
  end;

  TIpAbstractHtmlDataProvider = class(TIpBaseComponent)
  protected
    function DoGetHtmlStream(const URL: string;
      PostData: TIpFormDataEntity) : TStream; virtual; abstract;
    {-provider assumes ownership of returned TStream and will free it when
      done using it.}
    function DoCheckURL(const URL: string;
      var ContentType: string): Boolean; virtual; abstract;
    procedure DoLeave(Html: TIpHtml); virtual; abstract;
    procedure DoReference(const URL: string); virtual;  abstract;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture); virtual; abstract;
    function CanHandle(const URL: string): Boolean; virtual; abstract;
    // renamed New,Old for IP_LAZARUS to NewURL, OldURL
    function BuildURL(const OldURL, NewURL: string): string; virtual; abstract;
  end;

  TIpHtmlEnumerator = procedure(Document: TIpHtml) of object;

  TIpScrollAction = (hsaHome, hsaEnd, hsaPgUp, hsaPgDn,
    hsaLeft, hsaRight, hsaUp, hsaDown);
  
  TIpHtmlFrame = class
  protected
    CURURL : string;
    CurAnchor : string;
    FViewer: TIpHtmlCustomPanel;
    FNoScroll: Boolean;
    FramePanel : TPanel;
    Pnl : array[0..pred(IPMAXFRAMES)] of TPanel;
    FMarginWidth, FMarginHeight : Integer;
    FFlagErrors : Boolean;
    PostData : TIpFormDataEntity;
    Html : TIpHtml;
    HyperPanel : TIpHtmlInternalPanel;
    FrameCount : Integer;
    Frames : array[0..pred(IPMAXFRAMES)] of TIpHtmlFrame;
    FDataProvider : TIpAbstractHtmlDataProvider;
    FParent : TCustomPanel;
    Name : string;
    InOpen: Boolean;                                                   {!!.10}
    procedure InvalidateRect(Sender: TIpHtml; const R : TRect);
    procedure FramePanelResize(Sender: TObject);
    procedure AlignPanels;
    procedure InvalidateSize(Sender: TObject);
    procedure Get(Sender: TIpHtml; const URL:string);
    procedure Post(Sender: TIpHtml; const URL:string; FormData: TIpFormDataEntity); {!!.12}
    procedure IFrameCreate(Sender: TIpHtml; Parent: TWinControl;
      Frame: TIpHtmlNodeIFRAME;
      var Control: TWinControl);
    procedure InitHtml;
    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    procedure ControlClick(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    procedure ControlCreate(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    {$IFNDEF IP_LAZARUS}
    constructor Create(Viewer: TIpHtmlCustomPanel; Parent: TCustomPanel;
      DataProvider : TIpAbstractHtmlDataProvider; FlagErrors, NoScroll: Boolean;
      MarginWidth, MarginHeight: Integer);
    {$ENDIF}
    procedure OpenRelativeURL(const URL: string);
    procedure SelectAll;
    procedure DeselectAll;                                             {!!.10}
    procedure CopyToClipboard;
    function HaveSelection: Boolean;
    function FindFrame(const FrameName: string): TIpHtmlFrame;
    procedure MakeAnchorVisible(const URL: string);
    procedure Scroll(Action: TIpScrollAction);
    procedure Home;
    function IsExternal(const URL: string): Boolean;
    procedure SetHtml(NewHtml : TIpHtml);
    procedure Stop;
  public
    {$IFDEF IP_LAZARUS}
    // constructors should be public
    constructor Create(Viewer: TIpHtmlCustomPanel; Parent: TCustomPanel;
      DataProvider : TIpAbstractHtmlDataProvider; FlagErrors, NoScroll: Boolean;
      MarginWidth, MarginHeight: Integer);
    {$ENDIF}
    destructor Destroy; override;
    procedure OpenURL(const URL: string; Delayed: Boolean);
  end;

  TIpHtmlCustomScanner = class;
  TIpHtmlNVFrame = class
  protected
    CURURL : string;
    CurAnchor : string;
    FScanner: TIpHtmlCustomScanner;
    FFlagErrors : Boolean;
    PostData : TIpFormDataEntity;
    Html : TIpHtml;
    FrameCount : Integer;
    Frames : array[0..pred(IPMAXFRAMES)] of TIpHtmlNVFrame;
    FDataProvider : TIpAbstractHtmlDataProvider;
    Name : string;
    procedure InitHtml;
    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    {$IFNDEF IP_LAZARUS}
    constructor Create(Scanner: TIpHtmlCustomScanner;
      DataProvider : TIpAbstractHtmlDataProvider; FlagErrors: Boolean);
    {$ENDIF}
    procedure OpenRelativeURL(const URL: string);
    procedure SelectAll;
    procedure CopyToClipboard;
    function HaveSelection: Boolean;
    function FindFrame(const FrameName: string): TIpHtmlNvFrame;
    procedure MakeAnchorVisible(const URL: string);
    procedure Home;
    procedure Stop;
  public
    {$IFDEF IP_LAZARUS}
    // constructor should be public
    constructor Create(Scanner: TIpHtmlCustomScanner;
      DataProvider : TIpAbstractHtmlDataProvider; FlagErrors: Boolean);
    {$ENDIF}
    destructor Destroy; override;
    procedure OpenURL(const URL: string);
  end;

  TIpHtmlControlEvent = procedure(Sender: TIpHtmlCustomPanel;
    Frame: TIpHtmlFrame; Html: TIpHtml; Node: TIpHtmlNodeControl)
      of object;

  {!!.10 new}
  TIpHtmlPrintSettings = class(TPersistent)
  private
    FMarginTop: double;
    FMarginLeft: double;
    FMarginBottom: double;
    FMarginRight: double;
  protected
  public
    constructor Create;
    destructor Destroy; override;
  published
    property MarginLeft: double read FMarginLeft write FMarginLeft;
    property MarginTop: double read FMarginTop write FMarginTop;
    property MarginRight: double read FMarginRight write FMarginRight;
    property MarginBottom: double read FMarginBottom write FMarginBottom;
  end;

  { TIpHtmlCustomPanel }

  TIpHtmlCustomPanel = class(TCustomPanel)
  protected
    FFlagErrors: Boolean;
    FHotChange : TNotifyEvent;
    FHotClick : TNotifyEvent;
    FControlClick : TIpHtmlControlEvent;
    FControlCreate : TIpHtmlControlEvent;
    FCurElementChange: TNotifyEvent;                                   {!!.10}
    FDocumentOpen: TNotifyEvent;                                       {!!.10}
    FFixedTypeface: string;                                            {!!.10}
    {$IFDEF IP_LAZARUS}
    FDefaultTypeFace: string;
    {$ENDIF}
    FHotURL: string;
    FDataProvider: TIpAbstractHtmlDataProvider;
    URLStack : TStringList;
    TargetStack : TStringList;
    Stp : Integer;
    {CurURL : string;}                                                 {!!.12}
    VisitedList : TStringList;
    FVLinkColor: TColor;
    FLinkColor: TColor;
    FAlinkColor: TColor;
    FTextColor: TColor;
    FShowHints: Boolean;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    MasterFrame : TIpHtmlFrame;
    FHotNode : TIpHtmlNode;                                            {!!.12}
    FCurElement : PIpHtmlElement;
    GetURL : string;
    PostURL : string;
    PostData : TIpFormDataEntity;
    FAllowTextSelect: Boolean;
    FPrintSettings: TIpHtmlPrintSettings;                              {!!.10}
    procedure Push(const Target, URL: string);
    function GetTitle: string;
    procedure InternalOpenURL(const Target, HRef: string);
    procedure URLCheck(Sender: TIpHtml; const URL: string;
      var Visited: Boolean);
    procedure ReportURL(Sender: TIpHtml; const URL: string);
    procedure Paint; override;
    procedure HotChange(Sender: TObject);
    procedure CurElementChange(Sender: TObject);
    procedure HotClick(Sender: TObject);
    procedure ClientClick(Sender: TObject);
    procedure DoHotChange;
    procedure DoHotClick;
    procedure DoOnMouseWheel(Shift: TShiftState; Delta, XPos, YPos: SmallInt);  {!!.16}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMGetDlgCode(var Msg : TMessage); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMIpHttpGetRequest(var Message: TMessage); message CM_IpHttpGetRequest;
    procedure ControlClick(Frame: TIpHtmlFrame; Html: TIpHtml;
      Node: TIpHtmlNodeControl);
    procedure ControlCreate(Frame: TIpHtmlFrame; Html: TIpHtml;
      Node: TIpHtmlNodeControl);
    function GetVersion : string;
    procedure SetVersion(const Value : string);
    {$IFDEF IP_LAZARUS}
    procedure SetDefaultTypeFace(const Value: string);
    {$ENDIF}
  public
    function GetPrintPageCount: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;

    procedure CopyToClipboard;
    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    procedure GoBack;
    {$IFDEF IP_LAZARUS}
    function canGoBack : boolean;
    {$ENDIF}
    procedure GoForward;
    {$IFDEF IP_LAZARUS}
    function canGoForward : boolean;
    {$ENDIF}
    function HaveSelection: Boolean;
    property HotNode : TIpHtmlNode read FHotNode;                      {!!.12}
    function IsURLHtml(const URL: string): Boolean;
    procedure MakeAnchorVisible(const Name: string);
    {$IFDEF VERSION4}
    procedure MouseWheelHandler(Var Message: TMessage); Override;      {!!.16}
    {$ENDIF}
    procedure OpenURL(const URL: string);
    procedure Scroll(Action: TIpScrollAction);
    procedure SelectAll;
    procedure DeselectAll;                                             {!!.10}
    procedure SetHtml(NewHtml : TIpHtml);
    procedure Stop;

    procedure Print(FromPg, ToPg: LongInt);
    procedure PrintPreview;                                            {!!.10}

    property ALinkColor : TColor
                read FAlinkColor write FAlinkColor default clRed;
    property AllowTextSelect: Boolean
                read FAllowTextSelect write FAllowTextSelect
                default True;
    property CurElement : PIpHtmlElement read FCurElement;
    property DataProvider: TIpAbstractHtmlDataProvider
                read FDataProvider write FDataProvider;
    property FlagErrors : Boolean
                read FFlagErrors write FFlagErrors;
    property FixedTypeface: string
                read FFixedTypeface write FFixedTypeface;              {!!.10}
    {$IFDEF IP_LAZARUS}
    property DefaultTypeFace: string
                read FDefaultTypeFace write SetDefaultTypeFace;
    {$ENDIF}
    property HotURL : string read FHotURL;
    property LinkColor : TColor
                read FLinkColor write FLinkColor default clBlue;
    property MarginHeight : Integer
                read FMarginHeight write FMarginHeight default 10;
    property MarginWidth : Integer
                read FMarginWidth write FMarginWidth default 10;
    property PrintSettings : TIpHtmlPrintSettings                      {!!.10}
                   read FPrintSettings write FPrintSettings;           {!!.10}
    property ShowHints: Boolean
                read FShowHints write FShowHints default True;
    property TextColor : TColor
                read FTextColor write FTextColor default clBlack;
    property Title : string read GetTitle;
    property VLinkColor : TColor
                read FVLinkColor write FVLinkColor default clMaroon;

    property OnControlClick : TIpHtmlControlEvent
                read FControlClick write FControlClick;
    property OnControlCreate : TIpHtmlControlEvent
                read FControlCreate write FControlCreate;
    property OnCurElementChange: TNotifyEvent
                read FCurElementChange write FCurElementChange;        {!!.10}
    property OnDocumentOpen: TNotifyEvent
                read FDocumentOpen write FDocumentOpen;                {!!.10}
    property OnHotChange : TNotifyEvent
                read FHotChange write FHotChange;
    property OnHotClick : TNotifyEvent
                read FHotClick write FHotClick;
  published
    property Version : string
      read GetVersion write SetVersion stored False;
  end;

  TIpHtmlPanel = class(TIpHtmlCustomPanel)
  published
    property Align;
    property ALinkColor;
    property AllowTextSelect;
    {$IFDEF VERSION4}
    property Anchors;                                                  {!!.10}
    {$ENDIF}
    property BorderWidth;                                              {!!.10}
    property BorderStyle;                                              {!!.10}
    {$IFDEF VERSION4}
    property Constraints;                                              {!!.10}
    {$ENDIF}
    property DataProvider;
    property Enabled;                                                  {!!.10}
    property FixedTypeface;                                            {!!.10}
    {$IFDEF IP_LAZARUS}
    property DefaultTypeFace;
    {$ENDIF}
    property FlagErrors;
    property LinkColor;
    property MarginHeight;
    property MarginWidth;
    property PopupMenu;
    property PrintSettings;                                            {!!.10}
    property ShowHints;
    property TextColor;
    property Visible;                                                  {!!.10}
    property VLinkColor;
    {$IFDEF VERSION4}
    property OnCanResize;                                              {!!.10}
    {$ENDIF}
    property OnClick;
    {$IFDEF VERSION4}
    property OnConstrainedResize;                                      {!!.10}
    {$ENDIF}
    {$IFDEF VERSION5}
    property OnContextPopup;                                           {!!.10}
    {$ENDIF}
    property OnControlClick;
    property OnControlCreate;
    property OnCurElementChange;                                       {!!.10}
    property OnDocumentOpen;                                           {!!.10}
    property OnEnter;                                                  {!!.10}
    property OnExit;                                                   {!!.10}
    property OnHotChange;
    property OnHotClick;
  end;

  TIpHtmlCustomScanner = class(TComponent)
  protected
    FFlagErrors: Boolean;
    FDataProvider: TIpAbstractHtmlDataProvider;
    URLStack : TStringList;
    TargetStack : TStringList;
    Stp : Integer;
    CurURL : string;
    MasterFrame : TIpHtmlNVFrame;
    procedure Push(const Target, URL: string);
    function GetTitle: string;
    procedure InternalOpenURL(const Target, HRef: string);
    function GetVersion : string;                                      {!!.14}
    procedure SetVersion(const Value : string);                        {!!.14}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    function IsURLHtml(const URL: string): Boolean;
    procedure OpenURL(const URL: string);
    procedure Stop;

    property DataProvider: TIpAbstractHtmlDataProvider
                read FDataProvider write FDataProvider;
    property FlagErrors : Boolean
                read FFlagErrors write FFlagErrors;
    property Title : string read GetTitle;
{Begin !!.14}
  published
    property Version : string
      read GetVersion write SetVersion stored False;
{End !!.14}
  end;

  TIpHtmlScanner = class(TIpHtmlCustomScanner)
  published
    property DataProvider;
    property FlagErrors;
  end;

var
  ScaleFonts : Boolean = False; {true during print preview only}       {!!.10}
    {public to let print preview unit access it}
    
function MaxI2(const I1, I2: Integer) : Integer;
function MinI2(const I1, I2: Integer) : Integer;

function CalcMultiLength(const List: TIpHtmlMultiLengthList;
  Avail: Integer; var Sections: Integer): TIntArr;                     {!!.10}

{$IFDEF IP_LAZARUS}
procedure Register;
{$ENDIF}

implementation

uses
  Printers,
  IpHtmlPv; {!!.10}

var
  FlatSB_GetScrollInfo: function(hWnd: HWND; BarFlag: Integer;
    var ScrollInfo: TScrollInfo): BOOL; stdcall;
  FlatSB_GetScrollPos: function(hWnd: HWND; nBar: Integer): Integer; stdcall;
  FlatSB_SetScrollPos: function(hWnd: HWND; nBar, nPos: Integer;
    bRedraw: BOOL): Integer; stdcall;
  FlatSB_SetScrollProp: function(p1: HWND; index: Integer; newValue: Integer;
    p4: Bool): Bool; stdcall;
  FlatSB_SetScrollInfo: function(hWnd: HWND; BarFlag: Integer;
    const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;

const
  MaxElements = 1024*1024;
  ShyChar = #1; {character used to represent soft-hyphen in strings}
  NbspChar = #2; {character used to represent no-break space in strings}
  WheelDelta = 8;
const
  WSB_PROP_CYVSCROLL      = $00000001;
  WSB_PROP_CXHSCROLL      = $00000002;
  WSB_PROP_CYHSCROLL      = $00000004;
  WSB_PROP_CXVSCROLL      = $00000008;
  WSB_PROP_CXHTHUMB       = $00000010;
  WSB_PROP_CYVTHUMB       = $00000020;
  WSB_PROP_VBKGCOLOR      = $00000040;
  WSB_PROP_HBKGCOLOR      = $00000080;
  WSB_PROP_VSTYLE         = $00000100;
  WSB_PROP_HSTYLE         = $00000200;
  WSB_PROP_WINSTYLE       = $00000400;
  WSB_PROP_PALETTE        = $00000800;
  WSB_PROP_MASK           = $00000FFF;
  FSB_FLAT_MODE               = 2;
  FSB_ENCARTA_MODE            = 1;
  FSB_REGULAR_MODE            = 0;

var
  ScaleBitmaps : Boolean = False;                                      {!!.02}
  BWPrinter: Boolean;                                                  {!!.10}
  Aspect : double;                                                     {!!.02}

{$IFDEF IP_LAZARUS}
procedure DebugBox(Canvas: TCanvas; R: Trect; cl:TColor; dbg:boolean=false);
var
  OldPenColor: TColor;
begin
  OldPenColor := Canvas.Pen.Color;
  Canvas.Pen.Color := cl;
  Canvas.Moveto(r.left+(r.right-r.left) div 2, r.top);
  Canvas.Lineto(r.left+(r.right-r.left) div 2, r.bottom);
  Canvas.MoveTo(r.Left, r.top+(r.bottom-r.top) div 2);
  Canvas.LineTo(r.right, r.top+(r.bottom-r.top) div 2);
  if Dbg then
    DebugLn(dbgs(R));
  Canvas.Pen.Color := OldPenColor;
end;

procedure Register;
begin
  RegisterComponents('IPro', [TIpHtmlPanel]);
end;
{$ENDIF}

{!!.14 new}
{$IFNDEF VERSION3ONLY}
type
  THtmlRadioButton = class(TRadioButton)
  protected
    FChecked: Boolean;
    procedure SetChecked(Value: Boolean); override;
    function GetChecked: Boolean; override;
    procedure CreateWnd; override;
end;

{$IFDEF IP_LAZARUS}
const
  CheckStates: array[Boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
{$ENDIF}

procedure THtmlRadioButton.CreateWnd;
begin
  inherited CreateWnd;
  {$IFNDEF IP_LAZARUS}
  SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
  {$ENDIF}
end;

function THtmlRadioButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure THtmlRadioButton.SetChecked(Value: Boolean);
{$IFDEF IP_LAZARUS}
begin
  inherited SetChecked(Value);
end;
{$ELSE IP_LAZARUS}

  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
        for I := 0 to ControlCount - 1 do begin
          Sibling := Controls[I];
          if (Sibling <> Self)
          and (Sibling is THtmlRadioButton)
          and (Sibling.Tag = Self.Tag) then
            with THtmlRadioButton(Sibling) do
              SetChecked(False);
        end;
  end;

begin
  if FChecked <> Value then begin
    FChecked := Value;
    TabStop := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
    if Value then begin
      TurnSiblingsOff;
      inherited Changed;
      if not ClicksDisabled then
        Click;
    end;
  end;
end;
{$ENDIF IP_LAZARUS}

{$ENDIF}


{!!.02 new}
procedure GetRelativeAspect(PrinterDC : hDC);
var
  ScreenDC : hDC;
begin
  ScreenDC := GetDC(0);
  try
    Aspect := GetDeviceCaps(PrinterDC, LOGPIXELSX)
                / GetDeviceCaps(ScreenDC, LOGPIXELSX);
  finally
    ReleaseDC(0, ScreenDC);
  end;
end;

{$IFDEF IP_LAZARUS}
constructor TIpHtmlPoolManager.Create(TheItemSize, MaxItems : DWord);
begin
  inherited Create(TheItemSize);
  ClearOnCreate:=true;
end;

function TIpHtmlPoolManager.NewItm : Pointer;
begin
  Result:=NewItem;
end;

{$ELSE IP_LAZARUS}

constructor TIpHtmlPoolManager.Create(ItemSize, MaxItems : DWord);
begin
  InitializeCriticalSection(Critical);
  EnterCriticalSection(Critical);
  try
    InternalSize := ItemSize;
    while 4096 mod InternalSize <> 0 do
      inc(InternalSize);
    Root := VirtualAlloc(nil, InternalSize * MaxItems,
      MEM_RESERVE, PAGE_NOACCESS);
    NextPage := Root;
    Next := Root;
  finally
    LeaveCriticalSection(Critical);
  end;
  {Top := Pointer(DWord(Root) + InternalSize * MaxItems);}           {!!.12}
end;

destructor TIpHtmlPoolManager.Destroy;
begin
  EnterCriticalSection(Critical);
  try
    if Root <> nil then
      VirtualFree(Root, 0, MEM_RELEASE);
    inherited Destroy;
  finally
    LeaveCriticalSection(Critical);
  end;
  DeleteCriticalSection(Critical);
end;

function TIpHtmlPoolManager.NewItm : Pointer;
begin
  EnterCriticalSection(Critical);
  if Next = NextPage then
    Grow;
  Result := Next;
  inc(DWord(Next), InternalSize);
  LeaveCriticalSection(Critical);
end;

procedure TIpHtmlPoolManager.Grow;
var                                                                    {!!.10}
  P: Pointer;                                                          {!!.10}
begin
  P := VirtualAlloc(NextPage, 4096, MEM_COMMIT, PAGE_READWRITE);       {!!.10}
  if P = nil then                                                      {!!.10}
    raise Exception.Create('Out of memory');                           {!!.10}
  inc(DWord(NextPage),4096);
end;

procedure TIpHtmlPoolManager.EnumerateItems(Method: TIpEnumItemsMethod);
var
  P : Pointer;
begin
  P := Root;
  while DWord(P) < DWord(Next) do begin
    Method(P);
    inc(DWord(P), InternalSize);
  end;
end;
{$ENDIF IP_LAZARUS}


{$IFNDEF IP_LAZARUS}
// workaround for fpc bug: local string constants
function ParseConstant(const S: string): AnsiChar;
{$ENDIF}
Const
  CodeCount = 124;
  {Sorted by Size where size is Length(Name).
  Make sure you respect this when adding new items}
  Codes: array[0..pred(CodeCount)] of record
    Size: Integer;
    Name: String;
    Value: AnsiChar;
  end = (
    (Size: 2; Name: 'gt'; Value: '>'),
    (Size: 2; Name: 'lt'; Value: '<'),
    (Size: 3; Name: 'amp'; Value: '&'),
    (Size: 3; Name: 'deg'; Value: #176),
    (Size: 3; Name: 'ETH'; Value: #208),
    (Size: 3; Name: 'eth'; Value: #240),
    (Size: 3; Name: 'not'; Value: #172),
    (Size: 3; Name: 'reg'; Value: #174),
    (Size: 3; Name: 'shy'; Value: ShyChar),
    (Size: 3; Name: 'uml'; Value: #168),
    (Size: 3; Name: 'yen'; Value: #165),
    (Size: 4; Name: 'Auml'; Value: #196),
    (Size: 4; Name: 'auml'; Value: #228),
    (Size: 4; Name: 'bull'; Value: #149),
    (Size: 4; Name: 'cent'; Value: #162),
    (Size: 4; Name: 'circ'; Value: '^'),
    (Size: 4; Name: 'copy'; Value: #169),
    (Size: 4; Name: 'Euml'; Value: #203),
    (Size: 4; Name: 'euml'; Value: #235),
    (Size: 4; Name: 'fnof'; Value: #131),
    (Size: 4; Name: 'Iuml'; Value: #207),
    (Size: 4; Name: 'iuml'; Value: #239),
    (Size: 4; Name: 'macr'; Value: #175),
    (Size: 4; Name: 'nbsp'; Value: NbspChar),
    (Size: 4; Name: 'ordf'; Value: #170),
    (Size: 4; Name: 'ordm'; Value: #186),
    (Size: 4; Name: 'Ouml'; Value: #214),
    (Size: 4; Name: 'ouml'; Value: #246),
    (Size: 4; Name: 'para'; Value: #182),
    (Size: 4; Name: 'quot'; Value: '"'),
    (Size: 4; Name: 'sect'; Value: #167),
    (Size: 4; Name: 'sup1'; Value: #185),
    (Size: 4; Name: 'sup2'; Value: #178),
    (Size: 4; Name: 'sup3'; Value: #179),
    (Size: 4; Name: 'Uuml'; Value: #220),
    (Size: 4; Name: 'uuml'; Value: #252),
    (Size: 4; Name: 'Yuml'; Value: #159),
    (Size: 4; Name: 'yuml'; Value: #255),
    (Size: 5; Name: 'Acirc'; Value: #194),
    (Size: 5; Name: 'acirc'; Value: #226),
    (Size: 5; Name: 'acute'; Value: #180),
    (Size: 5; Name: 'AElig'; Value: #198),
    (Size: 5; Name: 'aelig'; Value: #230),
    (Size: 5; Name: 'Aring'; Value: #197),
    (Size: 5; Name: 'aring'; Value: #229),
    (Size: 5; Name: 'cedil'; Value: #184),
    (Size: 5; Name: 'Ecirc'; Value: #202),
    (Size: 5; Name: 'ecirc'; Value: #234),
    (Size: 5; Name: 'frasl'; Value: '/'),
    (Size: 5; Name: 'Icirc'; Value: #206),
    (Size: 5; Name: 'icirc'; Value: #238),
    (Size: 5; Name: 'iexcl'; Value: #161),
    (Size: 5; Name: 'laquo'; Value: #171),
    (Size: 5; Name: 'ldquo'; Value: #147),
    (Size: 5; Name: 'lsquo'; Value: #145),
    (Size: 5; Name: 'mdash'; Value: #151),
    (Size: 5; Name: 'micro'; Value: #181),
    (Size: 5; Name: 'minus'; Value: '-'),
    (Size: 5; Name: 'ndash'; Value: #150),
    (Size: 5; Name: 'Ocirc'; Value: #212),
    (Size: 5; Name: 'ocirc'; Value: #244),
    (Size: 5; Name: 'OElig'; Value: #140),
    (Size: 5; Name: 'oelig'; Value: #156),
    (Size: 5; Name: 'pound'; Value: #163),
    (Size: 5; Name: 'raquo'; Value: #187),
    (Size: 5; Name: 'rdquo'; Value: #148),
    (Size: 5; Name: 'rsquo'; Value: #146),
    (Size: 5; Name: 'szlig'; Value: #223),
    (Size: 5; Name: 'THORN'; Value: #222),
    (Size: 5; Name: 'thorn'; Value: #254),
    (Size: 5; Name: 'tilde'; Value: '~'),
    (Size: 5; Name: 'times'; Value: #215),
    (Size: 5; Name: 'trade'; Value: #153),
    (Size: 5; Name: 'Ucirc'; Value: #219),
    (Size: 5; Name: 'ucirc'; Value: #251),
    (Size: 6; Name: 'Aacute'; Value: #193),
    (Size: 6; Name: 'aacute'; Value: #225),
    (Size: 6; Name: 'Agrave'; Value: #192),
    (Size: 6; Name: 'agrave'; Value: #224),
    (Size: 6; Name: 'Atilde'; Value: #195),
    (Size: 6; Name: 'atilde'; Value: #227),
    (Size: 6; Name: 'brvbar'; Value: #166),
    (Size: 6; Name: 'Ccedil'; Value: #199),
    (Size: 6; Name: 'ccedil'; Value: #231),
    (Size: 6; Name: 'curren'; Value: #164),
    (Size: 6; Name: 'dagger'; Value: #134),
    (Size: 6; Name: 'Dagger'; Value: #135),
    (Size: 6; Name: 'divide'; Value: #247),
    (Size: 6; Name: 'Eacute'; Value: #201),
    (Size: 6; Name: 'eacute'; Value: #233),
    (Size: 6; Name: 'Egrave'; Value: #200),
    (Size: 6; Name: 'egrave'; Value: #232),
    (Size: 6; Name: 'frac12'; Value: #189),
    (Size: 6; Name: 'frac14'; Value: #188),
    (Size: 6; Name: 'frac34'; Value: #190),
    (Size: 6; Name: 'hellip'; Value: #133),
    (Size: 6; Name: 'Iacute'; Value: #205),
    (Size: 6; Name: 'iacute'; Value: #237),
    (Size: 6; Name: 'Igrave'; Value: #204),
    (Size: 6; Name: 'igrave'; Value: #236),
    (Size: 6; Name: 'iquest'; Value: #191),
    (Size: 6; Name: 'lsaquo'; Value: #139),
    (Size: 6; Name: 'middot'; Value: #183),
    (Size: 6; Name: 'Ntilde'; Value: #209),
    (Size: 6; Name: 'ntilde'; Value: #241),
    (Size: 6; Name: 'Oacute'; Value: #211),
    (Size: 6; Name: 'oacute'; Value: #243),
    (Size: 6; Name: 'Ograve'; Value: #210),
    (Size: 6; Name: 'ograve'; Value: #242),
    (Size: 6; Name: 'Oslash'; Value: #216),
    (Size: 6; Name: 'oslash'; Value: #248),
    (Size: 6; Name: 'Otilde'; Value: #213),
    (Size: 6; Name: 'otilde'; Value: #245),
    (Size: 6; Name: 'permil'; Value: #137),
    (Size: 6; Name: 'plusmn'; Value: #177),
    (Size: 6; Name: 'rsaquo'; Value: #155),
    (Size: 6; Name: 'Scaron'; Value: #138),
    (Size: 6; Name: 'scaron'; Value: #154),
    (Size: 6; Name: 'Uacute'; Value: #218),
    (Size: 6; Name: 'uacute'; Value: #250),
    (Size: 6; Name: 'Ugrave'; Value: #217),
    (Size: 6; Name: 'ugrave'; Value: #249),
    (Size: 6; Name: 'Yacute'; Value: #221),
    (Size: 6; Name: 'yacute'; Value: #253)
    );
{$IFDEF IP_LAZARUS}
function ParseConstant(const S: string): AnsiChar;
{$ENDIF}
var
  Error: Integer;
  Index1: Integer;
  Index2: Integer;
  Size1: Integer;
  Found: Boolean;
begin {'Complete boolean eval' must be off}
  Result := ' ';
  Size1 := Length(S);
  if Size1 = 0 then Exit;
  if (S[1] in ['$', '0'..'9']) then
  begin
    Val(S, Index1, Error);
    if (Error = 0) and (Index1 >= 32) and (Index1 <= 255) then
      Result := Chr(Index1);
  end else
  begin
    Index1 := 0;
    repeat
      if Size1 = Codes[Index1].Size then
      begin
        Found := True;
        Index2 := 1;
        while Index2 <= Size1 do
        begin
          if S[Index2] <> Codes[Index1].Name[Index2] then
          begin
            Found := False;
            Break;
          end;
          Inc(Index2);
        end;
        if Found then
        begin
          Result := Codes[Index1].Value;
          Break;
        end;
      end;
      Inc(Index1);
    until (Index1 >= CodeCount) or (Codes[Index1].Size > Size1);
  end;
end;

procedure ExpandEscapes(var S: string);
{- returns the string with & escapes expanded}
var
  i, j : Integer;
  Co : string;
  Ch : AnsiChar;
begin
  i := length(S);
  while i > 0 do begin
    if S[i] = '&' then begin
      j := i;
      while (j < length(S)) and not (S[j] in [';',' ']) do
        inc(j);
      Co := copy(S, i + 1, j - i - 1);
      if Co <> '' then begin
        if Co[1] = '#' then begin
          Delete(Co, 1, 1);
          if UpCase(Co[1]) = 'X' then begin
            Delete(Co, 1, 1);
            Insert('$', Co, 1);
          end;
        end;
        Ch := ParseConstant(Co);
        Delete(S, i, j - i + 1);
        Insert(Ch, S, i);
      end;
    end;
    dec(i);
  end;
end;

function EscapeToAnsi(const S: string): string;
var
  P : Integer;
begin
  Result := S;
  P := CharPos('&', S);
  if P <> 0 then
    ExpandEscapes(Result);
end;

function NoBreakToSpace(const S: string): string;
var
  P : Integer;
begin
  Result := S;
  for P := length(Result) downto 1 do
    if Result[P] = NbspChar then
      Result[P] := ' ';
end;

procedure SetRawWordValue(Entry: PIpHtmlElement; const Value: string);
var
  L : Integer;
begin
  Entry.AnsiWord := EscapeToAnsi(Value);
  Entry.IsBlank := 0;
  L := length(Entry.AnsiWord);
  while Entry.IsBlank < L do
    if Entry.AnsiWord[Entry.IsBlank + 1] = ' ' then
      inc(Entry.IsBlank)
    else
      break;
  if Entry.IsBlank < L  then
    Entry.IsBlank := 0;
end;

procedure SetWordRect(Element: PIpHtmlElement; const Value: TRect);
begin
  Element.WordRect2 := Value;
  if Element.ElementType = etObject then begin
    if (Value.Left < Value.Right)
    and (Value.Bottom > Value.Top)
    and (Value.Left >= 0) and (Value.Top >= 0) then
      TIpHtmlNodeAlignInline(Element.Owner).SetRect(Value);
  end;
end;

type
  TFriendPanel = class(TCustomPanel) end;

const
  LF = #10;
  CR = #13;                                                            {!!.10}
  {StdIndent = 16;}                                                    {!!.10}
  NullRect : TRect = (Left:0; Top:0; Right:0; Bottom:0);

{$IFNDEF IP_LAZARUS}
{$R IpHtml.res}
{$EndIf}

{!!.10 new}
function StdIndent: Integer;
begin
  if ScaleBitmaps and (Aspect > 0) then                                {!!.12}
    Result := round(16 * Aspect)
  else
    Result := 16;
end;

function SizeRec(cx, cy: Integer): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;

function MaxI2(const I1, I2: Integer) : Integer;
begin
  Result := I1;
  if I2 > I1 then
    Result := I2;
end;

function MaxI3(const I1, I2, I3: Integer) : Integer;
begin
  if I2 > I1 then
    if I3 > I2 then
      Result := I3
    else
      Result := I2
  else
    if I3 > I1 then
      Result := I3
    else
      Result := I1;
end;

function MinI2(const I1, I2: Integer) : Integer;
begin
  Result := I1;
  if I2 < I1 then
    Result := I2;
end;

function SameDimensions(const R1, R2 : TRect): Boolean;
begin
  Result :=
    (
    ((R1.Bottom - R1.Top) = (R2.Bottom - R2.Top))
    or (R1.Top = R2.Top))
     and
    ((R1.Right - R1.Left) = (R2.Right - R2.Left));
end;

function FirstString(const S: string): string;
{- returns first string if a list - otherwise the string itself}
var
  P : Integer;
begin
  P := CharPos(',', S);
  if P = 0 then
    Result := S
  else
    Result := copy(S, 1, P - 1);
end;

{ TIpHtmlInteger }

constructor TIpHtmlInteger.Create(AValue: Integer);
begin
  FValue := AValue;
end;

procedure TIpHtmlInteger.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

function TIpHtmlInteger.GetValue: Integer;
begin
  if ScaleBitmaps then
    Result := round(FValue * Aspect)
  else
    Result := FValue;
end;

procedure TIpHtmlInteger.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    DoChange;
  end;
end;

{ TIpHtmlPixels }

procedure TIpHtmlPixels.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

function TIpHtmlPixels.GetValue: Integer;
begin
  if (PixelsType = hpAbsolute) and ScaleBitmaps then
    Result := round(FValue * Aspect)
  else
    Result := FValue;
end;

procedure TIpHtmlPixels.SetPixelsType(const Value: TIpHtmlPixelsType);
begin
  if Value <> FPixelsType then begin
    FPixelsType := Value;
    DoChange;
  end;
end;

procedure TIpHtmlPixels.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    DoChange;
  end;
end;

{ TIpHtmlRelSize }

procedure TIpHtmlRelSize.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

procedure TIpHtmlRelSize.SetSizeType(const Value: TIpHtmlRelSizeType);
begin
  if Value <> FSizeType then begin
    FSizeType := Value;
    DoChange;
  end;
end;

procedure TIpHtmlRelSize.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    DoChange;
  end;
end;

{ TIpHtmlLength }

procedure TIpHtmlLength.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

function TIpHtmlLength.GetLengthValue: Integer;
begin
  if (LengthType = hlAbsolute) and ScaleBitmaps then
    Result := round(FLengthValue * Aspect)
  else
    Result := FLengthValue;
end;

procedure TIpHtmlLength.SetLengthType(const Value: TIpHtmlLengthType);
begin
  if Value <> FLengthType then begin
    FLengthType := Value;
    DoChange;
  end;
end;

procedure TIpHtmlLength.SetLengthValue(const Value: Integer);
begin
  if Value <> FLengthValue then begin
    FLengthValue := Value;
    DoChange;
  end;
end;

{ TIpHtmlMultiLength }

function TIpHtmlMultiLength.GetLengthValue: Integer;
begin
  if (LengthType = hmlAbsolute) and ScaleBitmaps then
    Result := round(FLengthValue * Aspect)
  else
    Result := FLengthValue;
end;

{ TIpHtmlMultiLengthList }

procedure TIpHtmlMultiLengthList.AddEntry(Value: TIpHtmlMultiLength);
begin
  List.Add(Value);
end;

procedure TIpHtmlMultiLengthList.Clear;
begin
  while List.Count > 0 do begin
    TIpHtmlMultiLength(List[0]).Free;
    List.Delete(0);
  end;
end;

constructor TIpHtmlMultiLengthList.Create;
begin
  List := TList.Create;
end;

destructor TIpHtmlMultiLengthList.Destroy;
begin
  inherited;
  Clear;
  List.Free;
end;

function TIpHtmlMultiLengthList.GetEntries: Integer;
begin
  Result := List.Count;
end;

function TIpHtmlMultiLengthList.GetValues(
  Index: Integer): TIpHtmlMultiLength;
begin
  Result := TIpHtmlMultiLength(List[Index]);
end;

{ TIpHtmlNode }

function TIpHtmlNode.GetHint: string;
begin
  Result := '';
end;

constructor TIpHtmlNode.Create(ParentNode : TIpHtmlNode);
begin
  if assigned(ParentNode) then
    if ParentNode is TIpHtmlNodeMulti then
      TIpHtmlNodeMulti(ParentNode).FChildren.Add(Self)
    else
      raise EIpHtmlException.Create(SHtmlNotContainer);        {!!.02}
  FParentNode := ParentNode;
  if ParentNode <> nil then
    FOwner := ParentNode.Owner;
end;

destructor TIpHtmlNode.Destroy;
begin
  if ((Owner = nil) or not Owner.Destroying)
  and (FParentNode <> nil) then
    TIpHtmlNodeMulti(FParentNode).FChildren.Remove(Self);
end;

function TIpHtmlNode.PageRectToScreen(const Rect: TRect;
  var ScreenRect: TRect): Boolean;
{ -convert coordinates of rect passed in to screen coordinates and
  return false if entire rect is clipped}
var
  Tmp : TRect;
begin
  if (Rect.Left = 0) and (Rect.Right = 0) and
     (Rect.Top  = 0) and (Rect.Bottom = 0) then begin
    Result := False;
    exit;
  end;
  if not IntersectRect(Tmp, Rect, Owner.PageViewRect) then begin
    Result := False;
    exit;
  end;
  ScreenRect := Rect;
  with Owner.PageViewRect do
    OffsetRect(ScreenRect, -Left, -Top);
  with Owner.ClientRect do
    OffsetRect(ScreenRect, Left, Top);
  if not IntersectRect(Tmp, ScreenRect, Owner.ClientRect) then begin
    Result := False;
    exit;
  end;
  Result := True;
end;

procedure TIpHtmlNode.ScreenLine(
  StartPoint, EndPoint : TPoint;
  const Width : Integer;
  const Color : TColor);
var
  SaveWidth : Integer;
begin
  StartPoint := PagePtToScreen(StartPoint);
  EndPoint := PagePtToScreen(EndPoint);
  SaveWidth := Owner.Target.Pen.Width;
  Owner.Target.Pen.Width := Width;
  Owner.Target.Pen.Color := Color;
  Owner.Target.MoveTo(StartPoint.x, StartPoint.y);
  Owner.Target.LineTo(EndPoint.x, EndPoint.y);
  Owner.Target.Pen.Width := SaveWidth;
end;

procedure TIpHtmlNode.ScreenRect(
      R : TRect;
      const Color : TColor);
begin
  if PageRectToScreen(R, R) then begin
    {$IFDEF IP_LAZARUS}
    Owner.Target.Brush.Style := bsSolid;
    {$ENDIF}
    Owner.Target.Brush.Color := Color;
    Owner.Target.FrameRect(R);
  end;
end;
{$IFDEF IP_LAZARUS}
procedure TIpHtmlNode.ScreenFrame(
      R : TRect;
      Raised: boolean);
var
  SaveWidth: Integer;
  procedure DoLine(X1,Y1,X2,Y2: Integer; Clr: TColor);
  begin
    Owner.Target.Pen.Color := Clr;
    Owner.Target.Line(X1,Y1,X2,Y2);
    //Owner.Target.MoveTo(X1, Y1);
    //Owner.Target.LineTo(X2, Y2);
  end;
begin
  if PageRectToScreen(R, R) then
  with Owner.Target do begin
    Brush.Style := bsSolid;
    SaveWidth := Pen.Width;
    Pen.Width := 1;
    if Raised then begin
      DoLine(R.Left, R.Top, R.Right-1, R.Top, RGB(220,220,220)); // above
      DoLine(R.Right-1, R.Bottom-1, R.Left, R.Bottom-1, RGB(64,64,64)); // below
      DoLine(R.Left, R.Top, r.Left, R.Bottom-1, RGB(192,192,192)); // Left
      DoLine(R.Right-1, R.Bottom-1, R.Right-1, R.Top, RGB(128,128,128)); // Right
    end else begin
      DoLine(R.Left, R.Top, R.Right-1, R.Top, RGB(64,64,64)); // above
      DoLine(R.Right-1, R.Bottom-1, R.Left, R.Bottom-1,RGB(220,220,220) ); // below
      DoLine(R.Left, R.Top, r.Left, R.Bottom-1, RGB(128,128,128)); // Left
      DoLine(R.Right-1, R.Bottom-1, R.Right-1, R.Top, RGB(192,192,192)); // Right
    end;
    Pen.Color := SaveWidth;
  end;
end;
{$ENDIF}
procedure TIpHtmlNode.ScreenPolygon(
      Points : array of TPoint;
      const Color : TColor);
var
  Pt : TPoint;
  i : Integer;
  SaveColor : TColor;
begin
  for i := 0 to High(Points) do begin
    Pt := PagePtToScreen(Points[i]);
    Points[i] := Pt;
  end;
  Owner.Target.Pen.Color := Color;
  SaveColor := Owner.Target.Brush.Color;
  Owner.Target.Brush.Color := Color;
  Owner.Target.Polygon(Points);
  Owner.Target.Brush.Color := SaveColor;
end;

function TIpHtmlNode.PagePtToScreen(const Pt : TPoint): TPoint;
{-convert coordinates of point passed in to screen coordinates}
begin
  Result := Pt;
  with Owner.PageViewRect do begin
    dec(Result.x, Left);
    dec(Result.y, Top);
  end;
  with Owner.ClientRect do begin
    inc(Result.x, Left);
    inc(Result.y, Top);
  end;
end;

procedure TIpHtmlNode.ReportDrawRects(M: TRectMethod);
begin
end;

procedure TIpHtmlNode.ReportMapRects(M: TRectMethod);
begin
end;

procedure TIpHtmlNode.InvalidateSize;
begin
  if FParentNode = nil then
    Owner.InvalidateSize
  else
    FParentNode.InvalidateSize;
end;

procedure TIpHtmlNode.EnumChildren(EnumProc: TIpHtmlNodeEnumProc;
  UserData: Pointer);
begin
  EnumProc(Self, UserData);
end;

procedure TIpHtmlNode.SubmitRequest;
begin
  if FParentNode <> nil then
    FParentNode.SubmitRequest;
end;

procedure TIpHtmlNode.ResetRequest;
begin
  if FParentNode <> nil then
    FParentNode.ResetRequest;
end;

procedure TIpHtmlNode.ReportCurDrawRects(Owner: TIpHtmlNode; M : TRectMethod);
begin
  if FParentNode <> nil then
    FParentNode.ReportCurDrawRects(Owner, M);
end;

procedure TIpHtmlNode.AppendSelection(var S: string);
begin
end;

procedure TIpHtmlNode.CreateControl(Parent: TWinControl);
begin
end;

procedure TIpHtmlNode.Enqueue;
begin

end;

procedure TIpHtmlNode.EnqueueElement(const Entry: PIpHtmlElement);
begin
end;

{!!.10 new}
function TIpHtmlNode.ElementQueueIsEmpty: Boolean;
begin
  Result := True;
end;

procedure TIpHtmlNode.HideUnmarkedControl;
begin
end;

procedure TIpHtmlNode.ImageChange(NewPicture: TPicture);
begin
end;

procedure TIpHtmlNode.Invalidate;
begin
end;

procedure TIpHtmlNode.MakeVisible;
begin
end;

procedure TIpHtmlNode.SetProps(const RenderProps: TIpHtmlProps);
begin
end;

procedure TIpHtmlNode.UnmarkControl;
begin
end;

{!!.10 attribute support code - new}

function GetPropertyValue(PI: PPropInfo; const AObject: TObject): string;

  function GetPropType : PTypeInfo;
  begin
    {$IFDEF VERSION3}
    Result := PI.PropType^;
    {$ELSE}
    Result := PI.PropType;
    {$ENDIF}
  end;

  function GetIntegerProperty : string;
  begin
    Result := IntToStr(GetOrdProp(AObject, PI));
  end;

  function GetCharProperty : string;
  begin
    Result := Char(GetOrdProp(AObject, PI));
  end;

  function GetEnumProperty : string;
  begin
    Result := GetEnumName(GetPropType, GetOrdProp(AObject, PI));
  end;

  function GetFloatProperty : string;
  const
    Precisions : array[TFloatType] of Integer = (7, 15, 18, 18, 19);
  begin
    Result := FloatToStrF(GetFloatProp(AObject, PI), ffGeneral,
      Precisions[GetTypeData(GetPropType)^.FloatType], 0);
  end;

  function GetLStringProperty : string;
  begin
    Result := GetStrProp(AObject, PI);
  end;

  function GetWCharProperty : string;
  begin
    Result := Char(GetOrdProp(AObject, PI));
  end;

  function GetVariantProperty : string;
  begin
    {$IFDEF FPC}
    Result := AnsiString(GetVariantProp(AObject, PI));
    {$ELSE}
    Result := GetVariantProp(AObject, PI);
    {$ENDIF}
  end;

  function GetStringProperty : string;
  begin
    Result := GetStrProp(AObject, PI);
  end;

  type
    TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  function GetSetProperty : string;
  var
    TypeInfo : PTypeInfo;
    W        : Cardinal;
    I        : Integer;
  begin
    Result := '[';
    W := GetOrdProp(AObject, PI);
    TypeInfo := GetTypeData(GetPropType)^.CompType{$IFNDEF IP_LAZARUS}^{$ENDIF};
    for I := 0 to pred(sizeof(Cardinal) * 8) do
      if I in TCardinalSet(W) then begin
        if Length(Result) <> 1 then
          Result := Result + ',';
        Result := Result + GetEnumName(TypeInfo, I);
      end;
    Result := Result + ']';
  end;


begin
  Result := '??';
  case PI.PropType^.Kind of
    tkInteger      : Result := GetIntegerProperty;
    tkChar         : Result := GetCharProperty;
    tkEnumeration  : Result := GetEnumProperty;
    tkFloat        : Result := GetFloatProperty;
    tkLString      : Result := GetLStringProperty;
    tkWChar        : Result := GetWCharProperty;
    tkVariant      : Result := GetVariantProperty;
    tkString       : Result := GetStringProperty;
    tkSet          : Result := GetSetProperty;
    else
      Result := 'unsupported';
  end;
end;

procedure SetPropertyValueLow(PI: PPropInfo;
  const AObject: TObject; const NewValue: string);

  function GetPropType : PTypeInfo;
  begin
    {$IFDEF VERSION3}
    Result := PI.PropType^;
    {$ELSE}
    Result := PI.PropType;
    {$ENDIF}
  end;

  procedure SetIntegerProperty;
  begin
    SetOrdProp(AObject, PI, StrToInt(NewValue));
  end;

  procedure SetCharProperty;
  begin
    SetOrdProp(AObject, PI, ord(NewValue[1]));
  end;

  procedure SetEnumProperty;
  begin
    {$IFDEF VERSION5}
    SetEnumProp(AObject, PI, NewValue);
    {$ENDIF}
  end;

  procedure SetFloatProperty;
  begin
    SetFloatProp(AObject, PI, StrToFloat(NewValue));
  end;

  procedure SetStringProperty;
  begin
    SetStrProp(AObject, PI, NewValue);
  end;

  type
    TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  procedure SetSetProperty;
  begin
    {$IFDEF VERSION5}
    SetSetProp(AObject, PI, NewValue);
    {$ENDIF}
  end;

begin
  if not assigned(PI.SetProc) then
    raise Exception.Create('Property is read-only');
  case PI.PropType^.Kind of
    tkInteger      : SetIntegerProperty;
    tkChar         : SetCharProperty;
    tkEnumeration  : SetEnumProperty;
    tkFloat        : SetFloatProperty;
    tkLString      : SetStringProperty;
    tkString       : SetStringProperty;
    tkSet          : SetSetProperty;
    else
      raise Exception.Create('Unsupported attribute type');
  end;
end;

function GetPropertyList(C: TObject; IncludeValues, IncludeBlanks: Boolean): TStringList;
var
  LCount: Integer;
  LSize: Integer;
  PList : PPropList;
  I, J: Integer;
  S: string;
  SubList: TStringList;
  O: TObject;
begin
  Result := TStringList.Create;
  try
    if C.ClassInfo <> nil then begin
      LCount := GetPropList(C.ClassInfo, tkProperties, nil);
      LSize := LCount * SizeOf(Pointer);
      if LSize > 0 then begin
        GetMem(PList, LSize);
        try
          GetPropList(C.ClassInfo, tkProperties, PList);
          for I := 0 to LCount-1 do begin
            if PList^[I].PropType^.Kind = tkClass then begin
              {SubList := TStringList.Create;}                         {!!.12}
              SubList := nil;                                          {!!.12}
              try
                O := TObject(GetOrdProp(C, PList^[I]));
                SubList := GetPropertyList(O, IncludeValues, IncludeBlanks);
                for j := 0 to pred(SubList.Count) do
                  Result.Add(PList^[I]^.Name + '.' + SubList[j]);
              finally
                SubList.Free;
              end;
            end else begin
              if IncludeValues then begin
                S := GetPropertyValue(PList^[I], C);
                if IncludeBlanks or (S <> '') then
                  Result.Add(PList^[I]^.Name + '=' + S);
              end else
                Result.Add(PList^[I]^.Name);
            end;
          end;
        finally
          FreeMem(PList, LSize);
        end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure SetPropertyValue(C: TObject; PropPath: string; const NewValue: string);
var
  LCount: Integer;
  LSize: Integer;
  PList : PPropList;
  I, J: Integer;
  SubPropPath: string;
  O: TObject;
begin
  I := pos('=', PropPath);
  if I <> 0 then
    SetLength(PropPath, I - 1);
  PropPath := trim(PropPath);
  if PropPath = '' then
    exit;
  PropPath := UpperCase(PropPath);
  if C.ClassInfo <> nil then begin
    LCount := GetPropList(C.ClassInfo, tkProperties, nil);
    LSize := LCount * SizeOf(Pointer);
    if LSize > 0 then begin
      GetMem(PList, LSize);
      try
        GetPropList(C.ClassInfo, tkProperties, PList);
        for I := 0 to LCount-1 do begin
          if PList^[I].PropType^.Kind = tkClass then begin
            J := pos('.', PropPath);
            if J <> 0 then begin
              SubPropPath := copy(PropPath, 1, J - 1);
              if SubPropPath = UpperCase(PList^[I]^.Name) then begin
                O := TObject(GetOrdProp(C, PList^[I]));
                SetPropertyValue(O, copy(PropPath, J + 1, MAXINT), NewValue);
                exit;
              end;
            end;
          end else begin
            if PropPath = UpperCase(PList^[I]^.Name) then begin
              SetPropertyValueLow(PList^[I], C, NewValue);
              exit;
            end;
          end;
        end;
      finally
        FreeMem(PList, LSize);
      end;
    end;
  end;
  raise Exception.Create('Unknown property:' + PropPath);
end;

{!!.10 new}
procedure TIpHtmlNode.GetAttributes(Target: TStrings; IncludeValues, IncludeBlanks: Boolean);
var
  List : TStringList;
begin
  List := GetPropertyList(Self, IncludeValues, IncludeBlanks);
  try
    Target.Assign(List);
  finally
    List.Free;
  end;
end;

procedure TIpHtmlNode.SetAttributeValue(const AttrName, NewValue: string);
begin
  SetPropertyValue(Self, AttrName, NewValue);
end;

function TIpHtmlNode.ExpParentWidth: Integer;
begin
  if assigned(FParentNode) then
    Result := FParentNode.ExpParentWidth
  else
    Result := MAXINT;
end;

{ TIpHtmlNodeMulti }

constructor TIpHtmlNodeMulti.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FChildren := TList.Create;
end;

destructor TIpHtmlNodeMulti.Destroy;
var
  i : Integer;
begin
  if Owner.Destroying then begin
    for i := 0 to pred(FChildren.Count) do
      TIpHtmlNode(FChildren[I]).Free;
  end else
    while FChildren.Count > 0 do begin
      TIpHtmlNode(FChildren[FChildren.Count - 1]).Free;
    end;
  FChildren.Free;
  inherited Destroy;
end;

function TIpHtmlNodeMulti.GetChildNode(Index: Integer): TIpHtmlNode;
begin
  Result := TIpHtmlNode(FChildren[Index]);
end;

function TIpHtmlNodeMulti.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

procedure TIpHtmlNodeMulti.Enqueue;
var
  i : Integer;
begin
  for i := 0 to pred(FChildren.Count) do begin
    TIpHtmlNode(FChildren[i]).Enqueue;
  end;
end;

procedure TIpHtmlNodeMulti.SetProps(const RenderProps: TIpHtmlProps);
var
  i : Integer;
begin
  for i := 0 to pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).SetProps(RenderProps);
end;

procedure TIpHtmlNodeMulti.ReportDrawRects(M: TRectMethod);
var
  i : Integer;
begin
  for i := 0 to pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).ReportDrawRects(M);
end;

procedure TIpHtmlNodeMulti.ReportMapRects(M: TRectMethod);
var
  i : Integer;
begin
  for i := 0 to pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).ReportMapRects(M);
end;

procedure TIpHtmlNodeMulti.EnumChildren(EnumProc: TIpHtmlNodeEnumProc;
  UserData: Pointer);
var
  i : Integer;
begin
  inherited;
  for i := 0 to pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).EnumChildren(EnumProc, UserData);
end;

procedure TIpHtmlNodeMulti.AppendSelection(var S: string);
var
  i : Integer;
begin
  inherited;
  for i := 0 to pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).AppendSelection(S);
end;

{ TIpHtmlNodeBODY }

constructor TIpHtmlNodeBODY.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FBgColor := -1;
  FText := -1;
  FLink := -1;
  FVLink := -1;
  FALink := -1;
  Owner.Body := Self;
end;

procedure TIpHtmlNodeBODY.Render(
  const RenderProps: TIpHtmlProps);
var
  MaxX, MaxY: Integer;                                                 {!!.02}
  X, Y : Integer;
  P : TPoint;
begin
  if ScaleBitmaps then begin                                       {!!.10}
    Owner.Target.Brush.Color := clWhite;
    Owner.Target.FillRect(Owner.ClientRect);
  end else begin
    {$IFDEF IP_LAZARUS}
    if BackGround = '' then begin
      if BGColor <> -1 then begin
        Owner.Target.Brush.Color := BGColor;
        Owner.Target.FillRect(Owner.ClientRect);
      end else begin
        Owner.Target.Brush.Color := clWhite;
        Owner.Target.FillRect(Owner.ClientRect);
      end;
    end;
    {$ELSE}
    if BackGround = '' then begin
      Owner.Target.Brush.Color := clWhite;
      Owner.Target.FillRect(Owner.ClientRect);
    end;
    if BGColor <> -1 then begin
      Owner.Target.Brush.Color := BGColor;
      Owner.Target.FillRect(Owner.ClientRect);
    end;
    {$ENDIF}
    if Background <> '' then begin
      if BgPicture = nil then
        Owner.DoGetImage(Self, Owner.BuildPath(Background), BgPicture);
      if BgPicture <> nil then begin
        MaxX := MaxI2(PageRect.Right, Owner.ClientRect.Right);           {!!.02}
        MaxY := MaxI2(PageRect.Bottom, Owner.ClientRect.Bottom);         {!!.02}
        Y := 0;
        while (Y <= MaxY{PageRect.Bottom}) do begin                      {!!.02}
          if (Y < Owner.PageViewRect.Top - BgPicture.Height)
          or (Y > Owner.PageViewRect.Bottom) then
          else begin
            X := 0;
            while (X <= MaxX{PageRect.Right}) do begin                   {!!.02}
              P := PagePtToScreen(Point(X, Y));
              Owner.Target.Draw(P.X, P.Y, BgPicture.Graphic);
              inc(X, BgPicture.Width);
            end;
          end;
          inc(Y, BgPicture.Height);
        end;
      end else begin                                                   {!!.12}
        Owner.Target.Brush.Color := clWhite;                           {!!.12}
        Owner.Target.FillRect(Owner.ClientRect);                       {!!.12}
      end;                                                             {!!.12}
    end;
  end;
  inherited Render(RenderProps);
  {$IFDEF IP_LAZARUS}
  // restore style
  Owner.Target.Brush.Style:=bsSolid;
  {$ENDIF}
end;

destructor TIpHtmlNodeBODY.Destroy;
begin
  inherited;
  BgPicture.Free;
end;

procedure TIpHtmlNodeBODY.ImageChange(NewPicture: TPicture);
begin
  {$IFOPT C+}
  Owner.CheckImage(NewPicture);
  {$ENDIF}
  BgPicture.Free;
  BgPicture := NewPicture;
  Invalidate;
end;

procedure TIpHtmlNodeBODY.SetAlink(const Value: TColorRef);
begin
  if Value <> FAlink then begin
    Falink := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBODY.SetBackground(const Value: string);
begin
  if Value <> FBackground then begin
    Fbackground := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBODY.SetBgcolor(const Value: TColorRef);
begin
  if Value <> FBgColor then begin
    Fbgcolor := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBODY.SetLink(const Value: TColorRef);
begin
  if Value <> FLink then begin
    Flink := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBODY.SetText(const Value: TColorRef);
begin
  if Value <> FText then begin
    Ftext := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBODY.SetVlink(const Value: TColorRef);
begin
  if Value <> FVLink then begin
    Fvlink := Value;
    InvalidateSize;
  end;
end;

{ TIpHtml }

procedure TIpHtml.ClearCache;
var
  i : Integer;
begin
  for i := 0 to pred(PropACache.Count) do begin
    TIpHtmlPropA(PropACache[i]).Free;
  end;
  PropACache.Free;
  for i := 0 to pred(PropBCache.Count) do
    TIpHtmlPropB(PropBCache[i]).Free;
  PropBCache.Free;
end;

procedure TIpHtml.ResetCache;
var
  i : Integer;
begin
  for i := 0 to pred(PropACache.Count) do begin
    TIpHtmlPropA(PropACache[i]).FSizeOfSpaceKnown := False;
    TIpHtmlPropA(PropACache[i]).tmHeight := 0;
  end;
end;

procedure TIpHtml.AddWordEntry(const Value: string;
  Props: TIpHtmlProps; Owner: TIpHtmlNode);
var
  Entry :  PIpHtmlElement;
  L : Integer;
begin
  Entry := NewElement(etWord, Owner);
  Entry.Props := Props;
  Entry.AnsiWord := Value;
  Entry.IsBlank := 0;
  L := length(Entry.AnsiWord);
  while Entry.IsBlank < L do
    if Entry.AnsiWord[Entry.IsBlank + 1] = ' ' then
      inc(Entry.IsBlank)
    else
      break;
  if Entry.IsBlank < L  then
    Entry.IsBlank := 0;
  Owner.EnqueueElement(Entry);
end;

procedure TIpHtml.AddWord(Value: string;
  Props: TIpHtmlProps; Owner: TIpHtmlNode);
var
  P : Integer;
begin
  Value:= EscapeToAnsi(Value);
  P := CharPos(ShyChar, Value);
  if P = 0 then
    AddWordEntry(Value, Props, Owner)
  else begin
    while Value <> '' do begin
      AddWordEntry(copy(Value, 1, P - 1), Props, Owner);
      Delete(Value, 1, P);
      if Value <> '' then
        Owner.EnqueueElement(SoftHyphen);
      P := CharPos(ShyChar, Value);
      if P = 0 then
        P := length(Value) + 1;
    end;
  end;
end;

procedure TIpHtml.InvalidateRect(R: TRect);
begin
  if Assigned(FOnInvalidateRect) then
    FOnInvalidateRect(Self, R);
end;

procedure TIpHtml.Clear;
{- clear any contents}
var
  i : Integer;
begin
  {$IFDEF UseGifImageUnit}
  for i := 0 to pred(GifImages.Count) do
    if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
      TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic).PaintStop;
  {$ELSE}
  for i := 0 to pred(AnimationFrames.Count) do
    if TIpHtmlNodeIMG(AnimationFrames[i]).FPicture <> nil then
      TIpAnimatedGraphic(TIpHtmlNodeIMG(AnimationFrames[i]).FPicture.Graphic).
        AggressiveDrawing := False;
  {$ENDIF}
  ClearGifQueue;
  FHotNode := nil;
  FHtml.Free;
  FHtml := TIpHtmlNodeHtml.Create(nil);
  FHtml.FOwner := Self;
end;

function TIpHtml.NextChar : AnsiChar;
begin
  {$IFDEF IP_LAZARUS}
  Result:=#0;
  {$ENDIF}
  if CharStream.Read(Result, 1) = 0 then
    Result := #0
  else begin
    inc(GlobalPos);
    if Result = #10 then begin
      inc(LineNumber);
      LineOffset := 0;
    end else
      inc(LineOffset);
    {write(Result);}
  end;
end;

procedure TIpHtml.ReportError(const ErrorMsg: string);
begin
  raise Exception.CreateFmt(SHtmlLineError, [ErrorMsg, LineNumber, LineOffset]);
end;

procedure TIpHtml.ReportExpectedError(const ErrorMsg: string);
begin
  ReportError(ErrorMsg + SHtmlExp);
end;

procedure TIpHtml.ReportExpectedToken(const Token: TIpHtmlToken);
begin
  ReportExpectedError(IpHtmlTokens[Token]);
end;

procedure TIpHtml.ReportReferences(Node : TIpHtmlNode);
var
  i : Integer;
  S : string;
begin
  if Node is TIpHtmlNodeA then
    S := Trim(TIpHtmlNodeA(Node).HRef)
  else
  if Node is TIpHtmlNodeAREA then
    S := Trim(TIpHtmlNodeAREA(Node).HRef);

  if (S <> '') then
    ReportReference(S);

  if Node is TIpHtmlNodeMulti then
    for i := 0 to pred(TIpHtmlNodeMulti(Node).ChildCount) do
      ReportReferences(TIpHtmlNodeMulti(Node).ChildNode[i]);
end;

procedure TIpHtml.LoadFromStream(S: TStream);
var
  T : TIpHtmlToken;
begin
  HtmlTokenList := nil;
  DoneLoading := False;
  try
    HtmlTokenList := TStringList.Create;
    for t := low(IpHtmlTokens) to high(IpHtmlTokens) do
      HtmlTokenList.AddObject(IpHtmlTokens[T], TObject(Integer(T)));
    HtmlTokenList.Sorted := True;
    FHasFrames := False;
    Clear;
    CharStream := S;
    GlobalPos := 0;
    LineNumber := 1;
    LineOffset := 0;
    Parse;
    ReportReferences(HtmlNode);
  finally
    HtmlTokenList.Free;
    HtmlTokenList := nil;
    DoneLoading := True;
    FCanPaint := True;
  end;                      
end;

{
procedure TIpHtml.ParseDocType;
begin
  if CurToken = IpHtmlTagDOCTYPE then
    NextToken;
end;
}

function TIpHtml.GetChar : AnsiChar;
var
  Trimming,                                                            {!!.10}
  Done: Boolean;                                                       {!!.10}
begin                                                                  {!!.10}
  Trimming := False;                                                   {!!.10}
  repeat                                                               {!!.10}
    Done := True;
    if (CharSP > 0) then begin
      dec(CharSP);
      Result := CharStack[CharSP];
    end else begin
      Result := NextChar;
      {if FlagErrors then
        write(Result);}
    end;
    {!!.10 thru end: new}
    if (InPre = 0) and (CurToken <> IpHtmlTagPRE) then begin
      if (Result <= ' ') and (Result > #0) then begin
        if (Result < ' ') and LastWasClose then begin
          Done := False;
          Trimming := True;
        end else
          if Trimming then
            Done := False
          else
            if LastWasSpace then
              Done := False
            else begin
              Result := ' ';
              LastWasSpace := True;
            end;
      end else
        LastWasSpace := False;
    end;
  until Done;
  LastWasClose := Result = '>';
end;

procedure TIpHtml.PutChar(Ch : AnsiChar);
begin
  if (CharSP >= sizeof(CharStack)) then
    raise EIpHtmlException.Create(SHtmlCharStackOverfl);       {!!.02}
  CharStack[CharSP] := Ch;
  inc(CharSP);
end;

function AnsiToEscape(const S: string): string;
{- returns the string with & escapes}
var
  i : Integer;
begin
  Result := S;
  i := length(Result);
  while i > 0 do begin
    case Result[i] of
    ShyChar :
      begin
        Result[i] := '&';
        Insert('shy;', Result, i + 1);
      end;
    NbspChar :
      begin
        Result[i] := '&';
        Insert('nbsp;', Result, i + 1);
      end;
    '"' :
      begin
        Result[i] := '&';
        Insert('quot;', Result, i + 1);
      end;
    '&' :
      begin
        Insert('amp;', Result, i + 1);
      end;
    '<' :
      begin
        Result[i] := '&';
        Insert('lt;', Result, i + 1);
      end;
    '>' :
      begin
        Result[i] := '&';
        Insert('gt;', Result, i + 1);
      end;
    end;
    dec(i);
  end;
end;

procedure TIpHtml.PutToken(Token : TIpHtmlToken);
begin
  if HaveToken then
    raise EIpHtmlException.Create(SHtmlTokenStackOverfl);      {!!.02}
  TokenBuffer := Token;
  HaveToken := True;
end;

function TIpHtml.IsWhiteSpace: Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to TBW - 1 do
    if TokenStringBuf[i] > ' ' then
      exit;
  Result := True;
end;

procedure TrimFormattingPre(const S: string; Target: PAnsiChar);
var
  R, W : Integer;
begin
  r := 1;
  w := 0;
  while r <= length(S) do begin
    case S[r] of
    #13 :
      begin
        Target[w] := LF;
        inc(w);
      end;
    #10 :
      if (w = 0) or (Target[w - 1] <> LF) then begin
        Target[w] := LF;
        inc(w);
      end;
    #0..#8, #11..#12, #14..#31 :
      ;
    #9, #32 :
      begin
        Target[w] := ' ';
        inc(w);
      end;
    else
      begin
        Target[w] := S[r];
        inc(w);
      end;
    end;
    inc(r);
  end;
  Target[w] := #0;
end;

procedure TrimFormattingNormal(const S: string; Target: PAnsiChar);
var
  R, W : Integer;
begin
  r := 1;
  w := 0;
  while r <= length(S) do begin
    case S[r] of
    #0..#9, #11..#13, #14..#31 :
      ;
    #10 :
      if w > 1 then begin
        Target[w] := ' ';
        inc(w);
      end;
    #32 :
      begin
        Target[w] := ' ';
        inc(w);
      end;
    else
      begin
        Target[w] := S[r];
        inc(w);
      end;
    end;
    inc(r);
  end;
  Target[w] := #0;
  r := 0;
  w := 0;
  while Target[r] <> #0 do begin
    case Target[r] of
    ' ' :
      if (w = 0) or (Target[w - 1] <> ' ') then begin
        Target[w] := ' ';
        inc(w);
      end;
    else
      if w <> r then
        Target[w] := Target[r];
      inc(w);
    end;
    inc(r);
  end;
  Target[w] := #0;
end;

function TIpHtml.GetTokenString: string;
begin
  TokenStringBuf[TBW] := #0;
  Result := StrPas(TokenStringBuf);
end;

procedure TIpHtml.NextToken;
var
  ParmName : string;
  {ParmBuf : array[0..4095] of AnsiChar;}                              {!!.12}
  PBW : Integer;
  i : Integer;
  Ctl,
  InValue, InQuote, InAttr, SeenEqual,
  SeenQuotes, Done, EndFound : Boolean;
  QuoteChar : AnsiChar;
  Ch : AnsiChar;

  procedure AddParmChar(const Ch: AnsiChar);
  begin
    {!!.12 begin}
    if PBW >= ParmBufSize - 1 then begin
      inc(ParmBufSize, 4096);
      ReallocMem(ParmBuf, ParmBufSize);
    end;
    {!!.12 end}
    ParmBuf[PBW] := Ch;
    inc(PBW);
  end;

  function ParmString: string;
  begin
    if PBW = 0 then                                                    {!!.12}
      Result := ''                                                     {!!.12}
    else begin                                                         {!!.12}
      ParmBuf[PBW] := #0;
      Result := StrPas(ParmBuf);
      PBW := 0;
    end;                                                               {!!.12}
  end;

  procedure AddTokenChar(const Ch: AnsiChar);
  begin
    TokenStringBuf[TBW] := Ch;
    inc(TBW);
  end;

begin
  if HaveToken then begin
    CurToken := TokenBuffer;
    HaveToken := False;
    exit;
  end;
  QuoteChar := ' ';
  repeat
    TBW := 0;
    PBW := 0;
    ParmList.Clear;
    ValueList.Clear;
    Ch := GetChar;
    if Ch = #0 then begin
      CurToken := IpHtmlTagEof;
      exit;
    end;
    if Ch = '<' then begin
      Ch := GetChar;
      if Ch = '!' then begin
        if GetChar = '-' then begin
          if GetChar <> '-' then
            if FlagErrors then
              ReportError(SHtmlDashExp);
          Ch := GetChar;
          repeat
            while Ch <> '-' do begin
              if Ch = #0 then
                break;
              Ch := GetChar;
            end;
            if (Ch = #0) then
              break
            else begin
              Ch := GetChar;
              if Ch = #0 then
                break;
              if Ch = '-' then begin
                Ch := GetChar;
                while (Ch = '-') do                                    {!!.12}
                  Ch := GetChar;                                       {!!.12}
                {if (Ch = #0) or (Ch = '>') then
                  break;}                                              {!!.12}
                while not (Ch in [#0,'>']) do                          {!!.12}
                  Ch := GetChar;                                       {!!.12}
                break;                                                 {!!.12}
              end;
            end;
          until false;
          CurToken := IpHtmlTagComment;
        end else begin
          Ch := GetChar;
          while Ch <> '>' do
            Ch := GetChar;
          CurToken := IpHtmlTagComment;
        end;
      end else begin
        while Ch <> '>' do begin
          if Ch <= ' ' then begin
            Ch := ' ';
            break;
          end;
          if Ch in [#33..#255] then
            AddTokenChar(UpCase(Ch));
          Ch := GetChar;
        end;
        if Ch = ' ' then begin
          Ch := GetChar;
          {list :== [attr]* ">"}
          {attr :== [" "]* attr-name [attr-value]}
          {attr-value :== [" "]* "=" [" "]* value}
          {value :== ['"']* string ['"']*}
          InAttr := False;
          InValue := False;
          InQuote := False;
          SeenEqual := False;
          SeenQuotes := False;
          ParmName := '';
          PBW := 0;
          while True do begin
            case Ch of
            #0 : break;
            #1..#31 :
              if InAttr then begin
                InAttr := False;
                ParmName := UpperCase(ParmString);
                SeenEqual := False;
              end else
              if InValue then begin
                if ParmName <> '' then begin
                  ParmList.Add(UpperCase(ParmName));
                  ValueList.Add(ParmString);
                  ParmName := '';
                end;
                InValue := False;
                SeenEqual := False;
                SeenQuotes := False;
              end;
            ' ' :
              if InQuote then
                AddParmChar(' ')
              else
              if InAttr then begin
                InAttr := False;
                ParmName := UpperCase(ParmString);
                SeenEqual := False;
              end else
              if InValue then begin
                if ParmName <> '' then begin
                  ParmList.Add(UpperCase(ParmName));
                  ValueList.Add(ParmString);
                  ParmName := '';
                end;
                InValue := False;
                SeenEqual := False;
                SeenQuotes := False;
              end;
            '''' :
              if InQuote then
                if QuoteChar = '''' then
                  InQuote := False
                else
                  AddParmChar('''')
              else begin
                InQuote := True;
                SeenQuotes := True;
                QuoteChar := '''';
              end;
            '"' :
              if InQuote then
                if QuoteChar = '"' then
                  InQuote := False
                else
                  AddParmChar('"')
              else begin
                InQuote := True;
                SeenQuotes := True;
                QuoteChar := '"';
              end;
            '<', '>' :
              begin
                if InQuote then                                        {!!.01}
                  AddParmChar(Ch)                                      {!!.01}
                else begin                                             {!!.01}
                  if InValue then begin
                    if ParmName <> '' then begin
                      ParmList.Add(UpperCase(ParmName));
                      ValueList.Add(ParmString);
                      ParmName := '';
                    end;
                  end;
                  break;
                end;                                                   {!!.01}
              end;
            '=' :
              begin
                SeenEqual := True;
                if InAttr then begin
                  ParmName := ParmString;
                  InAttr := False;
                end else
                  if InValue then
                    AddParmChar(Ch)
              end;
            else
              if InAttr or InValue then
                AddParmChar(Ch)
              else
                if SeenEqual and (InQuote or not SeenQuotes) then begin
                  InValue := True;
                  AddParmChar(Ch);
                end else begin
                  if (ParmName <> '') and not SeenQuotes then begin
                    ParmList.Add(UpperCase(ParmName));
                    ValueList.Add(ParmName);
                  end;
                  ParmName := '';
                  AddParmChar(Ch);
                  SeenEqual := False;
                  SeenQuotes := False;
                  InValue := False;
                  InAttr := True;
                end;
            end;
            Ch := GetChar;
          end;
          if InAttr then begin
            ParmName := UpperCase(ParmString);
            if (ParmName <> '') then begin
              ParmList.Add(ParmName);
              ValueList.Add(ParmName);
            end;
          end;
        end;

        { Check if this is a token of the form <tok/> }
        if (TBW > 0) and (TokenStringBuf[TBW - 1] = '/') then begin
          {It is, set EndFound flag and convert to normal open token}
          EndFound := True;
          dec(TBW);
        end else
          EndFound := False;
        TokenStringBuf[TBW] := #0;
        CurToken := IpHtmlTagUnknown;
        i := HtmlTokenList.IndexOf(string(TokenStringBuf));
        if i <> -1 then
          CurToken := TIpHtmlToken(PtrInt(HtmlTokenList.Objects[i]));

        {If the token was a single terminated token ( <tok/>
         as opposed to normal a <tok></tok> sequence), we fake
         it by pushing a close token to match the open token
         which was mangled above where EndFound was set.}

        if (CurToken <> IpHtmlTagUnknown) and EndFound then
          if succ(CurToken) in IpEndTokenSet then
            PutToken(succ(CurToken));

        (*
        !!.10 logic moved inside GetChar
        {clear white space after tag}
        Ch := GetChar;
        if (InPre = 0) and (CurToken <> IpHtmlTagPRE) then begin  {!!.03}
          if CurToken in IpEndTokenSet then begin
            while (Ch > #0) and (Ch < #32) do
              Ch := GetChar;
          end else begin
            while (Ch > #0) and (Ch < #32) do
              Ch := GetChar;
          end;
        end;                                              {!!.03}
        PutChar(Ch);
        *)

      end;
    end else begin
      CurToken := IpHtmlTagText;
      repeat
        Done := True;
        Ctl := False;
        while Ch <> '<' do begin
          case Ch of
          #0 :
            break;
          #10,#13 :
            begin
              Ctl := True;
              if InPre > 0 then                                        {!!.10}
                AddTokenChar(Ch);
            end
          else
            AddTokenChar(Ch);
          end;
          Ch := GetChar;
        end;
        if Ch <> #0 then begin
          Ch := GetChar;
          while (Ch > #0) and (Ch < ' ') do                            {!!.10}
            Ch := GetChar;                                             {!!.10}
          case Ch of
          '/', '!', 'a'..'z','A'..'Z' :
            begin
              PutChar(Ch);
              PutChar('<');
            end
          else
            begin
              AddTokenChar('<');
              AddTokenChar(Ch);
              Done := False;
              Ch := GetChar;
            end;
          end;
        end;
        if (InPre = 0) and Ctl
        and IsWhiteSpace then
          CurToken := IpHtmlTagCOMMENT;
      until Done;
    end;
  until
    (CurToken <> IpHtmlTagCOMMENT)
  and ((CurToken <> IpHtmlTagText) or (InBlock > 0) or (InPre > 0)
    or not IsWhiteSpace);
end;

procedure TIpHtml.NextRealToken;
begin
  repeat
    NextToken;
  until CurToken <> IpHtmlTagText;
end;

procedure TIpHtml.NextNonBlankToken;
begin
  repeat
    NextToken;
  until (CurToken <> IpHtmlTagText)
  or not IsWhiteSpace;
end;

procedure TIpHtml.SkipTextTokens;
begin
  while CurToken = IpHtmlTagText do
    NextToken;
end;

procedure TIpHtml.EnsureClosure(const EndToken : TIpHtmlToken;
  const EndTokens : TIpHtmlTokenSet);
begin
  if CurToken = EndToken then
    NextToken
  else
  if CurToken in EndTokens then
  else
    if FlagErrors then
      ReportExpectedToken(EndToken);
end;

procedure TIpHtml.ParseTitle(Parent: TIpHtmlNode);
var
  B : PAnsiChar;
begin
  FTitleNode := TIpHtmlNodeTITLE.Create(Parent);
  NextToken;
  if CurToken = IpHtmlTagText then begin
    Getmem(B, length(GetTokenString) + 1);
    try
      TrimFormattingNormal(EscapeToAnsi(GetTokenString), B);
      FTitleNode.Title := B;
    finally
      Freemem(B);
    end;
    NextToken;
  end;
  if CurToken = IpHtmlTagTITLEend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagTITLEend);
end;

procedure TIpHtml.ParseStyle(ParentNode : TIpHtmlNode);
var
  CurStyle : TIpHtmlNodeSTYLE;
begin
  CurStyle := TIpHtmlNodeSTYLE.Create(ParentNode);
  with CurStyle do begin
    Media := FindAttribute('MEDIA');
    Title := FindAttribute('TITLE');
  end;
  NextToken;
  if CurToken <> IpHtmlTagSTYLEend then
    ParseText([IpHtmlTagSTYLEend], CurStyle);
  if CurToken = IpHtmlTagSTYLEend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagSTYLEend);
end;

procedure TIpHtml.ParseScript(Parent : TIpHtmlNode;
  const EndTokens : TIpHtmlTokenSet);
begin
  TIpHtmlNodeSCRIPT.Create(Parent);
  NextToken;
  if CurToken <> IpHtmlTagScriptEnd then
    repeat
      NextToken;
    until (CurToken = IpHtmlTagSCRIPTend)
      or (CurToken in EndTokens);                                      {!!.12}
  EnsureClosure(IpHtmlTagSCRIPTend, EndTokens);
end;

procedure TIpHtml.ParseNoscript(Parent : TIpHtmlNode);
var
  CurScript : TIpHtmlNodeNOSCRIPT;
begin
  CurScript := TIpHtmlNodeNOSCRIPT.Create(Parent);
  with CurScript do begin
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurScript, [IpHtmlTagNOSCRIPTend]);
  if CurToken = IpHtmlTagNOSCRIPTend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagNOSCRIPTend);
end;

procedure TIpHtml.ParseIsIndex;
begin
  IndexPhrase := FindAttribute('PROMPT');
  {IsIndexPresent := IndexPhrase <> '';}                               {!!.12}
  NextToken;
end;

procedure TIpHtml.ParseBase;
begin
  {Base := FindAttribute('HREF');}                                     {!!.12}
  NextToken;
end;

procedure TIpHtml.ParseMeta;
begin
  with TIpHtmlNodeMETA.Create(Parent) do begin
    HttpEquiv := FindAttribute('HTTP-EQUIV');
    Name := FindAttribute('NAME');
    Content := FindAttribute('CONTENT');
    Scheme := FindAttribute('SCHEME');
  end;
  NextToken;
end;

procedure TIpHtml.ParseLink;
begin
  with TIpHtmlNodeLINK.Create(Parent) do begin
    HRef := FindAttribute('HREF');
    Rel := FindAttribute('REL');
    Rev := FindAttribute('REV');
    Title := FindAttribute('TITLE');
    ParseBaseProps(Self);
  end;
  NextToken;
end;

procedure TIpHtml.ParseHeadItems(Parent : TIpHtmlNode);
begin
  while not (CurToken in
    [IpHtmlTagEOF, IpHtmlTagHEADend, IpHtmlTagFRAMESET, IpHtmlTagBODY]) do begin
    case CurToken of
    IpHtmlTagTITLE :
      ParseTitle(Parent);
    IpHtmlTagSTYLE :
      ParseStyle(Parent);
    IpHtmlTagSCRIPT :
      ParseScript(Parent, [IpHtmlTagEOF]);
    IpHtmlTagNOSCRIPT :
      ParseNoscript(Parent);
    IpHtmlTagISINDEX :
      ParseIsIndex;
    IpHtmlTagBASE :
      ParseBase;
    IpHtmlTagMETA :
      ParseMeta(Parent);
    IpHtmlTagLINK :
      ParseLink(Parent);
    else
      {unknown}
      NextToken;
    end;
  end;
end;

procedure TIpHtml.ParseHead(Parent : TIpHtmlNode);
begin
  {lead token is optional}
  if CurToken = IpHtmlTagHEAD then begin
    NextToken;
    ParseHeadItems(TIpHtmlNodeHEAD.Create(Parent));
    if CurToken = IpHtmlTagHEADend then
      NextToken;
  end;
end;

procedure TIpHtml.ParseFont(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  CurFONT : TIpHtmlNodeFONT;
begin
  CurFONT := TIpHtmlNodeFONT.Create(Parent);
  with CurFONT do begin                                                {!!.10}
    Face := FindAttribute('FACE');
    Size.Free;                                                         {!!.10}
    Size := ParseRelSize{('+0')};                                      {!!.10}
    Size.OnChange := SizeChanged;                                      {!!.10}
    Color := ColorFromString(FindAttribute('COLOR'));
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurFONT, EndTokens + [IpHtmlTagFONTend]);
  EnsureClosure(IpHtmlTagFONTend, EndTokens);
end;

procedure TIpHtml.ParsePre(ParentNode : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  CurContainer : TIpHtmlNodePRE;
begin
  CurContainer := TIpHtmlNodePRE.Create(ParentNode);
  CurContainer.ParseBaseProps(Self);
  inc(InPre);                                                          {!!.10}
  NextToken;
  {inc(InPre);}                                                        {!!.10}
  ParseBodyText(CurContainer, EndTokens + [IpHtmlTagPREend]);
  dec(InPre);
  EnsureClosure(IpHtmlTagPREend, EndTokens);
end;

procedure TIpHtml.ParseText(const EndTokens : TIpHtmlTokenSet;
  Parent: TIpHtmlNode);
var
  CurContainer : TIpHtmlNodeText;
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagEof :
      exit;
    {IpHtmlTagFont :
      begin
        ParseFont(Parent, EndTokens);
      end;}
    IpHtmlTagText :
      begin
        CurContainer := TIpHtmlNodeText.Create(Parent);
        if CurContainer=nil then ;
        CurContainer.FEscapedText := GetTokenString;
        NextToken;
      end;
    else
      NextToken;
    end;
  end;
end;

procedure TIpHtml.ParseHeader(Parent : TIpHtmlNode; EndToken : TIpHtmlToken;
  Size : Integer);
var
  NewHeader : TIpHtmlNodeHeader;
begin
  NewHeader := TIpHtmlNodeHeader.Create(Parent);
  NewHeader.ParseBaseProps(Self);
  NewHeader.Size := Size;
  NewHeader.Align := ParseAlignment;
  NextToken;
  ParseBodyText(NewHeader, [EndToken]);
  if CurToken = EndToken then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(EndToken);
end;

procedure TIpHtml.ParseParagraph(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  NewPara : TIpHtmlNodeP;
begin
  NewPara := TIpHtmlNodeP.Create(Parent);
  NewPara.ParseBaseProps(Self);
  NewPara.Align := ParseAlignment;
  NextToken;
  ParseBodyText(NewPara, EndTokens + [IpHtmlTagPend, IpHtmlTagP, IpHtmltagTABLE]);
  if CurToken = IpHtmlTagPend then
    NextToken
  else
  if CurToken in (EndTokens + [IpHtmlTagP, IpHtmltagTABLE]) then
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagPend);
end;

procedure TIpHtml.ParseAddress(Parent : TIpHtmlNode);
var
  NewPara : TIpHtmlNodeADDRESS;
begin
  NewPara := TIpHtmlNodeADDRESS.Create(Parent);
  NewPara.ParseBaseProps(Self);
  NextToken;
  ParseBodyText(NewPara, [IpHtmlTagADDRESSend]);
  if CurToken = IpHtmlTagADDRESSend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagADDRESSend);
end;

procedure TIpHtml.ParseListItems(Parent : TIpHtmlNodeCore;
        EndToken: TIpHtmlToken; const EndTokens : TIpHtmlTokenSet;
        DefaultListStyle : TIpHtmlULType);
var
  NewListItem : TIpHtmlNodeLI;
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagLI :
      begin
        NewListItem := TIpHtmlNodeLI.Create(Parent);
        NewListItem.ParseBaseProps(Self);
        {NewListItem.DefListType := DefaultListStyle;}                 {!!.12}
        NewListItem.ListType := ParseULStyle(DefaultListStyle);
        NewListItem.Value := ParseInteger('VALUE', -1);
        NewListItem.Compact := ParseBoolean('COMPACT');
        NextToken;
        ParseBodyText(NewListItem,
                      EndTokens + [EndToken, IpHtmlTagLI, IpHtmlTagLIend] -
                                  [IpHtmlTagP, IpHtmlTagPend]);
        if CurToken = IpHtmlTagLIend then
          NextToken;
        SkipTextTokens;                                                {!!.10}  
      end;
    else
      ParseBodyText(Parent, EndTokens + [EndToken, IpHtmlTagLI]);
    end;
  end;
end;

procedure TIpHtml.ParseUnorderedList(Parent: TIpHtmlNode;
  EndToken : TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
var
  NewList : TIpHtmlNodeList;
begin
  case pred(EndToken) of
  IpHtmlTagDIR : NewList := TIpHtmlNodeDIR.Create(Parent);
  IpHtmlTagMENU : NewList := TIpHtmlNodeMENU.Create(Parent);
  else {IpHtmlTagUL : }NewList := TIpHtmlNodeUL.Create(Parent);
  end;
  NewList.ParseBaseProps(Self);
  case ListLevel of
  0 : NewList.ListType := ParseULStyle(ulDisc);
  1 : NewList.ListType := ParseULStyle(ulCircle);
  else
    NewList.ListType := ParseULStyle(ulSquare);
  end;
  NewList.Compact := ParseBoolean('COMPACT');
  NextToken;
  inc(ListLevel);
  ParseListItems(NewList,
                 EndToken, EndTokens + [EndToken] - [IpHtmlTagP, IpHtmlTagLI],
                 NewList.ListType);
  dec(ListLevel);
  EnsureClosure(EndToken, EndTokens);
end;

procedure TIpHtml.ParseOrderedList(Parent: TIpHtmlNode;
  const EndTokens : TIpHtmlTokenSet);
var
  NewList : TIpHtmlNodeOL;
begin
  NewList := TIpHtmlNodeOL.Create(Parent);
  NewList.Style := ParseOLStyle(olArabic);
  NewList.Start := ParseInteger('START', 1);
  NewList.Compact := ParseBoolean('COMPACT');
  NextToken;
  ParseListItems(NewList, IpHtmlTagOLend, EndTokens + [IpHtmlTagOLend], ulDisc);
  EnsureClosure(IpHtmlTagOLend, EndTokens);
end;

function TIpHtml.ParseInputType : TIpHtmlInputType;
var
  S : string;
begin
  Result := hitText;
  S := UpperCase(FindAttribute('TYPE'));
  if (S = '') or (S = 'TEXT') or (S = 'TEXTAREA') then
  else
  if S = 'PASSWORD' then
    Result := hitPassword
  else
  if S = 'CHECKBOX' then
    Result := hitCheckbox
  else
  if S = 'RADIO' then
    Result := hitRadio
  else
  if S = 'SUBMIT' then
    Result := hitSubmit
  else
  if S = 'RESET' then
    Result := hitReset
  else
  if S = 'FILE' then
    Result := hitFile
  else
  if S = 'HIDDEN' then
    Result := hitHidden
  else
  if S = 'IMAGE' then
    Result := hitImage
  else
  if S = 'BUTTON' then
    Result := hitButton
  else
    if FlagErrors then
      ReportError(SHtmlInvType);
end;

function TIpHtml.ParseButtonType : TIpHtmlButtonType;
var
  S : string;
begin
  Result := hbtSubmit;
  S := UpperCase(FindAttribute('TYPE'));
  if (S = '') or (S = 'SUBMIT') then
  else
  if S = 'RESET' then
    Result := hbtReset
  else
  if S = 'BUTTON' then
    Result := hbtButton
  else
    if FlagErrors then
      ReportError(SHtmlInvType);
end;

procedure TIpHtml.ParseFormFields(Parent: TIpHtmlNode;
  const EndTokens : TIpHtmlTokenSet);
var
  CurSelect : TIpHtmlNodeSELECT;
  CurTextArea : TIpHtmlNodeTEXTAREA;
  CurButton : TIpHtmlNodeBUTTON;
  CurOptGroup : TIpHtmlNodeOPTGROUP;
  CurLabel : TIpHtmlNodeLABEL;
  CurFieldset : TIpHtmlNodeFIELDSET;
  CurLegend : TIpHtmlNodeLEGEND;
  CurOption : TIpHtmlNodeOPTION;
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagINPUT :
      begin
        with TIpHtmlNodeINPUT.Create(Parent) do begin
          ParseBaseProps(Self);
          InputType := ParseInputType;
          Name := FindAttribute('NAME');
          Value := FindAttribute('VALUE');
          Checked := ParseBoolean('CHECKED');
          Size := ParseInteger('SIZE', -1);
          MaxLength := ParseInteger('MAXLENGTH', -1);
          Src := FindAttribute('SRC');
          Align := ParseImageAlignment('BOTTOM');
          Disabled := ParseBoolean('DISABLED');
          ReadOnly := ParseBoolean('READONLY');
          Alt := FindAttribute('ALT');
          TabIndex := ParseInteger('TABINDEX', -1);
        end;
        NextToken;
      end;
    IpHtmlTagBUTTON :
      begin
        CurButton := TIpHtmlNodeBUTTON.Create(Parent);
        with CurButton do begin
          ParseBaseProps(Self);
          ButtonType := ParseButtonType;
          Name := FindAttribute('NAME');
          Value := FindAttribute('VALUE');
          Disabled := ParseBoolean('DISABLED');
          TabIndex := ParseInteger('TABINDEX', -1);
        end;
        NextToken;
        ParseBodyText(CurButton, EndTokens + [IpHtmlTagBUTTONend]);
        if CurToken = IpHtmlTagBUTTONend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagBUTTONend);
      end;
    IpHtmlTagSELECT :
      begin
        CurSelect := TIpHtmlNodeSELECT.Create(Parent);
        with CurSelect do begin
          Name := FindAttribute('NAME');
          Size := ParseInteger('SIZE', -1);
          ParseBaseProps(Self);
          Multiple := ParseBoolean('MULTIPLE');
          Disabled := ParseBoolean('DISABLED');
          TabIndex := ParseInteger('TABINDEX', -1);
        end;
        NextNonBlankToken;
        repeat
          case CurToken of
          IpHtmlTagOPTION :
            begin
              CurOption := TIpHtmlNodeOPTION.Create(CurSelect);
              with CurOption do begin
                ParseBaseProps(Self);
                Selected := ParseBoolean('SELECTED');
                Value := FindAttribute('VALUE');
                Disabled := ParseBoolean('DISABLED');
                OptionLabel := FindAttribute('LABEL');
              end;
              NextNonBlankToken;
              ParseText(EndTokens +
                        [IpHtmlTagSelectEND, IpHtmlTagOption, IpHtmlTagOPTIONend],
                        CurOption);
              if CurToken = IpHtmlTagOPTIONend then
                NextNonBlankToken;
            end;
          IpHtmlTagOPTGROUP :
            begin
              CurOptGroup := TIpHtmlNodeOPTGROUP.Create(CurSelect);
              with CurOptGroup do begin
                ParseBaseProps(Self);
                Disabled := ParseBoolean('DISABLED');
                GroupLabel := FindAttribute('LABEL');
              end;
              NextNonBlankToken;
              while CurToken = IpHtmlTagOPTION do begin
                CurOption := TIpHtmlNodeOPTION.Create(CurOptGroup);
                with CurOption do begin
                  ParseBaseProps(Self);
                  Selected := ParseBoolean('SELECTED');
                  Value := FindAttribute('VALUE');
                  Disabled := ParseBoolean('DISABLED');
                  OptionLabel := FindAttribute('LABEL');
                end;
                NextNonBlankToken;
                ParseText(EndTokens +
                          [IpHtmlTagSelectEND, IpHtmlTagOption, IpHtmlTagOPTIONend],
                          CurOption);
                if CurToken = IpHtmlTagOPTIONend then
                  NextNonBlankToken;
              end;
              if CurToken = IpHtmlTagOPTGROUPend then
                NextNonBlankToken
              else
              if CurToken = IpHtmlTagOPTGROUP then
              else
              if CurToken = IpHtmlTagOPTION then
              else
              if CurToken = IpHtmlTagSELECTend then
              else
                if FlagErrors then
                  ReportExpectedToken(IpHtmlTagOPTGROUPend);
            end;
          else
            break;
          end;
        until False;
        if CurToken = IpHtmlTagSELECTend then
          NextNonBlankToken;
      end;
    IpHtmlTagTEXTAREA :
      begin
        CurTextArea := TIpHtmlNodeTEXTAREA.Create(Parent);
        with CurTextArea do begin
          Name := FindAttribute('NAME');
          Rows := ParseInteger('ROWS', 20);
          Cols := ParseInteger('COLS', 20);
          ParseBaseProps(Self);
          Disabled := ParseBoolean('DISABLED');
          ReadOnly := ParseBoolean('READONLY');
          TabIndex := ParseInteger('TABINDEX', -1);
        end;
        NextToken;
        ParseText([IpHtmlTagTEXTAREAend], CurTextArea);
        if CurToken = IpHtmlTagTEXTAREAend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagTEXTAREAend);
      end;
    IpHtmlTagLABEL :
      begin
        CurLabel := TIpHtmlNodeLABEL.Create(Parent);
        with CurLabel do begin
          ParseBaseProps(Self);
          LabelFor := FindATTRIBUTE('LABEL');
        end;
        NextToken;
        ParseBodyText(CurLabel, [IpHtmlTagLABELend]);
        if CurToken = IpHtmlTagLABELend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagLABELend);
      end;
    IpHtmlTagFIELDSET :
      begin
        CurFieldset := TIpHtmlNodeFIELDSET.Create(Parent);
        with CurFieldset do
          ParseBaseProps(Self);
        NextToken;
        ParseFormFields(CurFieldSet, EndTokens + [IpHtmlTagFIELDSETend]);
        if CurToken = IpHtmlTagFIELDSETend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagFIELDSETend);
      end;
    IpHtmlTagLEGEND :
      begin
        CurLegend := TIpHtmlNodeLEGEND.Create(Parent);
        with CurLegend do begin
          ParseBaseProps(Self);
        end;
        NextToken;
        ParseBodyText(CurLegend, [IpHtmlTagLEGENDend]);
        if CurToken = IpHtmlTagLEGENDend then
          NextToken
        else
          if FlagErrors then
            ReportExpectedToken(IpHtmlTagLEGENDend);
      end;
    else
      exit;
    end;
  end;
end;

procedure TIpHtml.ParseForm(Parent: TIpHtmlNode;
        const EndTokens: TIpHtmlTokenSet);
var
  NewForm : TIpHtmlNodeFORM;
begin
  NewForm := TIpHtmlNodeFORM.Create(Parent);
  with NewForm do begin
    Action := FindAttribute('ACTION');
    Method := ParseMethod;
    Enctype := FindAttribute('ENCTYPE');
    Name := FindAttribute('NAME');
    AcceptCharset := FindAttribute('ACCEPT-CHARSET');
    Accept := FindAttribute('ACCEPT');
    if Enctype = '' then
      Enctype := 'application/x-www-form-urlencoded';
    if AcceptCharset = '' then
      AcceptCharset := 'UNKNOWN';
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(NewForm, EndTokens + [IpHtmlTagFORMend]);
  EnsureClosure(IpHtmlTagFORMend, EndTokens);
end;

procedure TIpHtml.ParseDefListItems(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  CurDT : TIpHtmlNodeDT;
  CurDD : TIpHtmlNodeDD;
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagDT :
      begin
        CurDT := TIpHtmlNodeDT.Create(Parent);
        CurDT.ParseBaseProps(Self);
        NextToken;
        ParseBodyText(CurDT, [IpHtmlTagDD, IpHtmlTagDTend] + EndTokens);
        if CurToken = IpHtmlTagDTend then
          NextToken;
      end;
    IpHtmlTagDD :
      begin
        CurDD := TIpHtmlNodeDD.Create(Parent);
        CurDD.ParseBaseProps(Self);
        NextToken;
        ParseBodyText(CurDD, [IpHtmlTagDT, IpHtmlTagDDend] + EndTokens);
        if CurToken = IpHtmlTagDDend then
          NextToken;
      end;
    else
      ParseBodyText(Parent, EndTokens + [IpHtmlTagDT, IpHtmlTagDD]);
    end;
  end;
end;

procedure TIpHtml.ParseDefinitionList(Parent: TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  NewDL : TIpHtmlNodeDL;
begin
  NewDL := TIpHtmlNodeDL.Create(Parent);
  NewDL.ParseBaseProps(Self);
  NewDL.Compact := ParseBoolean('COMPACT');
  NextToken;
  ParseDefListItems(NewDL, EndTokens + [IpHtmlTagDLend]);
  EnsureClosure(IpHtmlTagDLend, EndTokens);
end;

procedure TIpHtml.ParseDIV(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  CurDIV : TIpHtmlNodeDIV;
begin
  CurDIV := TIpHtmlNodeDIV.Create(Parent);
  with CurDIV do begin
    Align := ParseAlignment;
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurDIV, EndTokens + [IpHtmlTagDIVend]);
  EnsureClosure(IpHtmlTagDIVend, EndTokens);
end;

procedure TIpHtml.ParseSPAN(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  CurSPAN : TIpHtmlNodeSPAN;
begin
  CurSPAN := TIpHtmlNodeSPAN.Create(Parent);
  with CurSPAN do begin
    Align := ParseAlignment;
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurSPAN, EndTokens + [IpHtmlTagSPANend]);
  EnsureClosure(IpHtmlTagSPANend, EndTokens);
end;

procedure TIpHtml.ParseCENTER(Parent : TIpHtmlNode;
          const EndTokens: TIpHtmlTokenSet);
var
  CurContainer : TIpHtmlNodeDIV;
begin
  CurContainer := TIpHtmlNodeDIV.Create(Parent);
  with CurContainer do
    Align := haCenter;
  NextToken;
  ParseBodyText(CurContainer, EndTokens + [IpHtmlTagCENTERend]);
  EnsureClosure(IpHtmlTagCENTERend, EndTokens);
end;

procedure TIpHtml.ParseLEFT(Parent : TIpHtmlNode;
          const EndTokens: TIpHtmlTokenSet);
var
  CurContainer : TIpHtmlNodeDIV;
begin
  CurContainer := TIpHtmlNodeDIV.Create(Parent);
  with CurContainer do
    Align := haLeft;
  NextToken;
  ParseBodyText(CurContainer, EndTokens + [IpHtmlTagLEFTend]);
  EnsureClosure(IpHtmlTagLEFTend, EndTokens);
end;

procedure TIpHtml.ParseRIGHT(Parent : TIpHtmlNode;
          const EndTokens: TIpHtmlTokenSet);
var
  CurContainer : TIpHtmlNodeDIV;
begin
  CurContainer := TIpHtmlNodeDIV.Create(Parent);
  with CurContainer do
    Align := haRight;
  NextToken;
  ParseBodyText(CurContainer, EndTokens + [IpHtmlTagRIGHTend]);
  EnsureClosure(IpHtmlTagRIGHTend, EndTokens);
end;

procedure TIpHtml.ParseBLINK(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  CurBlink : TIpHtmlNodeBLINK;
begin
  CurBlink := TIpHtmlNodeBLINK.Create(Parent);
  NextToken;
  ParseBodyText(CurBlink, EndTokens + [IpHtmlTagBLINKend]);
  EnsureClosure(IpHtmlTagBLINKend, EndTokens);
end;

procedure TIpHtml.ParseBLOCKQUOTE(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  BQ : TIpHtmlNodeBLOCKQUOTE;
begin
  BQ := TIpHtmlNodeBLOCKQUOTE.Create(Parent);
  BQ.ParseBaseProps(Self);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagBLOCKQUOTEend]);
  EnsureClosure(IpHtmlTagBLOCKQUOTEend, EndTokens);
end;

procedure TIpHtml.ParseQ(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  BQ : TIpHtmlNodeQ;
begin
  BQ := TIpHtmlNodeQ.Create(Parent);
  BQ.ParseBaseProps(Self);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagQend]);
  EnsureClosure(IpHtmlTagQend, EndTokens);
end;

procedure TIpHtml.ParseINS(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  BQ : TIpHtmlNodeINS;
begin
  BQ := TIpHtmlNodeINS.Create(Parent);
  BQ.ParseBaseProps(Self);
  BQ.Cite := FindAttribute('CITE');
  BQ.Datetime := FindAttribute('DATETIME');
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagINSend]);
  EnsureClosure(IpHtmlTagINSend, EndTokens);
end;

procedure TIpHtml.ParseDEL(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  BQ : TIpHtmlNodeDEL;
begin
  BQ := TIpHtmlNodeDEL.Create(Parent);
  BQ.ParseBaseProps(Self);
  BQ.Cite := FindAttribute('CITE');
  BQ.Datetime := FindAttribute('DATETIME');
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagDELend]);
  EnsureClosure(IpHtmlTagDELend, EndTokens);
end;

procedure TIpHtml.ParseFontStyle(Parent : TIpHtmlNode;
  StartToken : TIpHtmlToken; const EndTokens : TIpHtmlTokenSet);
var
  CurStyle : TIpHtmlNodeFontStyle;
begin
  CurStyle := TIpHtmlNodeFontStyle.Create(Parent);
  case StartToken of
  IpHtmlTagTT :
    CurStyle.Style := hfsTT;
  IpHtmlTagI :
    CurStyle.Style := hfsI;
  IpHtmlTagB :
    CurStyle.Style := hfsB;
  IpHtmlTagU :
    CurStyle.Style := hfsU;
  IpHtmlTagSTRIKE :
    CurStyle.Style := hfsSTRIKE;
  IpHtmlTagS :
    CurStyle.Style := hfsS;
  IpHtmlTagBIG :
    CurStyle.Style := hfsBIG;
  IpHtmlTagSMALL :
    CurStyle.Style := hfsSMALL;
  IpHtmlTagSUB :
    CurStyle.Style := hfsSUB;
  IpHtmlTagSUP :
    CurStyle.Style := hfsSUP;
  end;
  CurStyle.ParseBaseProps(Self);
  NextToken;
  ParseBodyText(CurStyle, EndTokens);
  EnsureClosure(succ(StartToken), EndTokens);
end;

procedure TIpHtml.ParseHR(Parent : TIpHtmlNode);
var
  NewRule : TIpHtmlNodeHR;
begin
  NewRule := TIpHtmlNodeHR.Create(Parent);
  with NewRule do begin
    Align := ParseImageAlignment('CENTER');
    NoShade := ParseBoolean('NOSHADE');
    Size := ParseHtmlInteger('SIZE', 1);                               {!!.10}
    Size.OnChange := WidthChanged;                                     {!!.10}
    Width := ParseHyperLength('WIDTH', '100%');
    Width.OnChange := WidthChanged;                                    {!!.10}
    Color := ColorFromString(FindAttribute('COLOR'));
    ParseBaseProps(Self);
  end;
  NextToken;
end;

procedure TIpHtml.ParseBR(Parent : TIpHtmlNode);
var
  BR : TIpHtmlNodeBR;
begin
  BR := TIpHtmlNodeBR.Create(Parent);
  BR.Clear := ParseBRClear;
  BR.Id := FindAttribute('ID');
  BR.ClassId :=FindAttribute('CLASS');
  BR.Title :=FindAttribute('TITLE');
  BR.Style :=FindAttribute('STYLE');
  NextToken;
end;

procedure TIpHtml.ParseNOBR(Parent : TIpHtmlNode);
begin
  NextToken;
  ParseBodyText(TIpHtmlNodeNOBR.Create(Parent), [IpHtmlTagNOBRend]);
  if CurToken = IpHtmlTagNOBRend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagNOBRend);
end;


procedure TIpHtml.ParsePhraseElement(Parent : TIpHtmlNode;
  StartToken, EndToken : TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
var
  CurPhrase : TIpHtmlNodePhrase;
begin
  NextToken;
  CurPhrase := TIpHtmlNodePhrase.Create(Parent);
  case StartToken of
  IpHtmlTagEM :
    CurPhrase.Style := hpsEM;
  IpHtmlTagSTRONG :
    CurPhrase.Style := hpsSTRONG;
  IpHtmlTagDFN :
    CurPhrase.Style := hpsDFN;
  IpHtmlTagCODE :
    CurPhrase.Style := hpsCODE;
  IpHtmlTagSAMP :
    CurPhrase.Style := hpsSAMP;
  IpHtmlTagKBD :
    CurPhrase.Style := hpsKBD;
  IpHtmlTagVAR :
    CurPhrase.Style := hpsVAR;
  IpHtmlTagCITE :
    CurPhrase.Style := hpsCITE;
  IpHtmlTagABBR :
    CurPhrase.Style := hpsABBR;
  IpHtmlTagACRONYM :
    CurPhrase.Style := hpsACRONYM;
  end;
  CurPhrase.ParseBaseProps(Self);
  ParseBodyText(CurPhrase, [EndToken] + EndTokens);
  if CurToken = EndToken then
    NextToken
  else
  if CurToken in EndTokens then
  else
    if FlagErrors then
      ReportExpectedToken(EndToken);
end;

procedure TIpHtml.ParseAnchor(Parent : TIpHtmlNode;
  const EndTokens : TIpHtmlTokenSet);
var
  CurAnchor : TIpHtmlNodeA;
begin
  CurAnchor := TIpHtmlNodeA.Create(Parent);
  with CurAnchor do begin
    Name := FindAttribute('NAME');
    HRef := FindAttribute('HREF');
    Rel := FindAttribute('REL');
    Rev := FindAttribute('REV');
    Title := FindAttribute('TITLE');
    ParseBaseProps(Self);
    Shape := ParseShape;
    TabIndex := ParseInteger('TABINDEX', -1);
    Target := FindAttribute('TARGET');
  end;
  NextToken;
  ParseBodyText(CurAnchor, EndTokens + [IpHtmlTagAend] - [IpHtmlTagA]);
  if CurToken = IpHtmlTagAend then
    NextToken
  else
  if CurToken = IpHtmlTagA then
  else
  if CurToken in EndTokens then
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagAend);
  if (CurAnchor.ChildCount = 0)
  and (CurAnchor.Name <> '') then
    TIpHtmlNodeText.Create(CurAnchor).FEscapedText := '&nbsp;';
end;

procedure TIpHtml.ParseIMG(Parent : TIpHtmlNode);
var
  CurIMG : TIpHtmlNodeIMG;
begin
  CurIMG := TIpHtmlNodeIMG.Create(Parent);
  with CurIMG do begin
    Src := FindAttribute('SRC');
    Alt := FindAttribute('ALT');
    Align := ParseImageAlignment('BOTTOM');
    Height := ParsePixels('HEIGHT', '');                               {!!.10}
      {ParseInteger('HEIGHT', -1);}                                    {!!.10}
    Height.OnChange := DimChanged;                                     {!!.10}
    Width := ParseHyperLength('WIDTH', '');                            {!!.10}
    Width.OnChange := DimChanged;                                      {!!.10}
    Border := ParseInteger('BORDER', 0);
    HSpace := ParseInteger('HSPACE', 0);
    VSpace := ParseInteger('VSPACE', 0);
    UseMap := FindAttribute('USEMAP');
    IsMap := ParseBoolean('ISMAP');
    ParseBaseProps(Self);
    LongDesc := FindAttribute('LONGDESC');
    Name := FindAttribute('NAME');
  end;
  NextToken;
end;

procedure TIpHtml.ParseApplet(Parent: TIpHtmlNode;
        const EndTokens : TIpHtmlTokenSet);
var
  CurApplet : TIpHtmlNodeAPPLET;
  CurParam : TIpHtmlNodePARAM;
begin
  CurApplet := TIpHtmlNodeAPPLET.Create(Parent);
  with CurApplet do begin
    Codebase := FindAttribute('CODEBASE');
    Code := FindAttribute('CODE');
    Alt := FindAttribute('ALT');
    Name := FindAttribute('NAME');
    Height := ParseInteger('HEIGHT', -1);
    Width := ParseHyperLength('WIDTH', '');
    Width.OnChange := WidthChanged;                                    {!!.10}
    Align := ParseImageAlignment('BOTTOM');
    HSpace := ParseInteger('HSPACE', 1);
    VSpace := ParseInteger('VSPACE', 1);
    Archive := FindAttribute('ARCHIVE');
    ObjectCode := FindAttribute('OBJECT');
    Id := FindAttribute('ID');
    ClassID := FindAttribute('CLASS');
    Title := FindAttribute('TITLE');
    Style := FindAttribute('STYLE');
  end;
  NextToken;
  while not (CurToken in EndTokens + [IpHtmlTagAPPLETend]) do begin
    case CurToken of
    IpHtmlTagPARAM :
      begin
        CurParam := TIpHtmlNodePARAM.Create(CurApplet);                {!!.12}
        with CurParam do begin
          {CurParam := TIpHtmlNodePARAM.Create(CurApplet);}            {!!.12}
          Name := FindAttribute('NAME');
          Value := FindAttribute('VALUE');
          Id := FindAttribute('ID');
          ValueType := ParseObjectValueType;
        end;
        NextToken;
      end;
    else
      ParseText([IpHtmlTagAPPLETend, IpHtmlTagPARAM], CurApplet);
    end;
  end;
  EnsureClosure(IpHtmlTagAPPLETend, EndTokens);
end;

procedure TIpHtml.ParseOBJECT(Parent : TIpHtmlNode);
var
  CurOBJECT : TIpHtmlNodeOBJECT;
  CurParam : TIpHtmlNodePARAM;
begin
  CurOBJECT := TIpHtmlNodeOBJECT.Create(Parent);
  with CurOBJECT do begin
    ClassID := FindAttribute('CLASSID');
    Codebase := FindAttribute('CODEBASE');
    Data := FindAttribute('DATA');
    CodeType := FindAttribute('CODETYPE');
    Archive := FindAttribute('ARCHIVE');
    Standby := FindAttribute('STANDBY');
    Align := ParseImageAlignment('BOTTOM');
    Height := ParseInteger('HEIGHT', -1);
    Width := ParseHyperLength('WIDTH', '');
    Width.OnChange := WidthChanged;                                    {!!.10}
    Border := ParseInteger('BORDER', 0);
    HSpace := ParseInteger('HSPACE', 1);
    VSpace := ParseInteger('VSPACE', 1);
    UseMap := FindAttribute('USEMAP');
    Declare := ParseBoolean('DECLARE');
    ParseBaseProps(Self);
    Name := FindAttribute('NAME');
  end;
  NextToken;
  while not (CurToken = IpHtmlTagOBJECTend) do begin
    case CurToken of
    IpHtmlTagPARAM :
      begin
        CurParam := TIpHtmlNodePARAM.Create(CurObject);
        with CurParam do begin
          Name := FindAttribute('NAME');
          Value := FindAttribute('VALUE');
          Id := FindAttribute('ID');
          ValueType := ParseObjectValueType;
        end;
        NextToken;
      end;
    else
      ParseText([IpHtmlTagOBJECTend, IpHtmlTagPARAM], CurObject);
    end;
  end;
  if CurToken = IpHtmlTagOBJECTend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagOBJECTend);
end;

procedure TIpHtml.ParseTableRow(Parent: TIpHtmlNode;
  const EndTokens : TIpHtmlTokenSet);
var
  CurHeader : TIpHtmlNodeTH;
  CurTableCell : TIpHtmlNodeTD;
begin
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagTH :
      begin
        CurHeader := TIpHtmlNodeTH.Create(Parent);
        with CurHeader do begin
          Nowrap := ParseBoolean('NOWRAP');
          Rowspan := ParseInteger('ROWSPAN', 1);
          Colspan := ParseInteger('COLSPAN', 1);
          ParseBaseProps(Self);
          Align := ParseCellAlign(haCenter{haDefault});
          VAlign := ParseVAlignment3;
          Width := ParseHyperLength('WIDTH', '');
          Width.OnChange := DimChanged;                                {!!.10}
          Height := ParsePixels('HEIGHT', '');                         {!!.10}
            {ParseInteger('HEIGHT', -1);}                              {!!.10}
          Height.OnChange := DimChanged;
          BgColor := ColorFromString(FindAttribute('BGCOLOR'));
        end;
        NextToken;
        ParseBodyText(CurHeader,
                      EndTokens + [IpHtmlTagTH, IpHtmlTagTHend, IpHtmlTagTD]);
        if CurToken in [IpHtmlTagTHend, IpHtmlTagTDend] then
          NextRealToken;
      end;
    IpHtmlTagTD :
      begin
        CurTableCell := TIpHtmlNodeTD.Create(Parent);
        with CurTableCell do begin
          Nowrap := ParseBoolean('NOWRAP');
          Rowspan := ParseInteger('ROWSPAN', 1);
          Colspan := ParseInteger('COLSPAN', 1);
          ParseBaseProps(Self);
          Align := ParseCellAlign(haLeft{haDefault});
          VAlign := ParseVAlignment3;
          Width := ParseHyperLength('WIDTH', '');
          Width.OnChange := DimChanged;                                {!!.10}
          Height := ParsePixels('HEIGHT', '');                         {!!.10}
            {ParseInteger('HEIGHT', -1);}                              {!!.10}
          Height.OnChange := DimChanged;
          BgColor := ColorFromString(FindAttribute('BGCOLOR'));
        end;
        NextToken;
        ParseBodyText(CurTableCell, EndTokens + [IpHtmlTagTD, IpHtmlTagTDend]);
        if CurToken = IpHtmlTagTDend then
          NextRealToken;
      end;
    else
      NextToken;
    end;
  end;
end;

procedure TIpHtml.ParseTableRows(Parent: TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);

  {!!.12 new}
  procedure FixupPercentages(CurRow: TIpHtmlNodeTR);
  var
    i, Pt, P0: Integer;
  begin
    Pt := 0;
    P0 := 0;
    for i := 0 to CurRow.ChildCount - 1 do
      if CurRow.ChildNode[i] is TIpHtmlNodeTableHeaderOrCell then
        case TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width.LengthType of
        hlUndefined :
          inc(P0);
        hlPercent :
          inc(Pt, TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width.LengthValue);
        end;
    if (Pt > 0) and (Pt < 100) and (P0 > 0) then begin
      Pt := (100 - Pt) div P0;
      for i := 0 to CurRow.ChildCount - 1 do
        if CurRow.ChildNode[i] is TIpHtmlNodeTableHeaderOrCell then
          if TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width.LengthType = hlUndefined then begin
           TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width.LengthType := hlPercent;
           TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width.LengthValue := Pt;
          end;
    end;
  end;

var
  CurRow : TIpHtmlNodeTR;
begin
  CurRow := nil;                                                       {!!.12}
  while not (CurToken in EndTokens) do
    case CurToken of
      IpHtmlTagTR :
        begin
          if CurRow <> nil then                                        {!!.12}
            FixupPercentages(CurRow);                                  {!!.12}
          CurRow := TIpHtmlNodeTR.Create(Parent);
          CurRow.ParseBaseProps(Self);
          CurRow.Align := ParseAlignment;
          CurRow.VAlign := ParseVAlignment;
          NextRealToken;
          ParseTableRow(CurRow,
                        EndTokens + [IpHtmlTagTRend, IpHtmlTagTR] -
                                    [IpHtmlTagTH, IpHtmlTagTD]);
          while CurToken = IpHtmlTagTRend do
            NextToken;
        end;
      IpHtmlTagTH,
      IpHtmlTagTD :
        begin
          if CurRow <> nil then                                        {!!.12}
            FixupPercentages(CurRow);                                  {!!.12}
          CurRow := TIpHtmlNodeTR.Create(Parent);
          ParseTableRow(CurRow,
                        EndTokens + [IpHtmlTagTR] - [IpHtmlTagTH, IpHtmlTagTD]);
        end;
      else
        NextToken;
    end;
  if CurRow <> nil then                                        {!!.12}
    FixupPercentages(CurRow);                                  {!!.12}
end;

procedure TIpHtml.ParseTableBody(Parent: TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  CurHead : TIpHtmlNodeTHEAD;
  CurFoot : TIpHtmlNodeTFOOT;
  CurBody : TIpHtmlNodeTBODY;
begin
  if CurToken = IpHtmlTagTHEAD then begin
    CurHead := TIpHtmlNodeTHEAD.Create(Parent);
    CurHead.ParseBaseProps(Self);
    CurHead.Align := ParseCellAlign(haLeft);
    CurHead.VAlign := ParseVAlignment3;
    NextToken;
    ParseTableRows(CurHead,
                   EndTokens + [IpHtmlTagTFOOT, IpHtmlTagTBODY, IpHtmlTagTHEADend] -
                               [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD]);
    if CurToken = IpHtmlTagTHEADend then
      NextToken;
  end;
  if CurToken = IpHtmlTagTFOOT then begin
    CurFoot := TIpHtmlNodeTFOOT.Create(Parent);
    CurFoot.ParseBaseProps(Self);
    CurFoot.Align := ParseCellAlign(haLeft);
    CurFoot.VAlign := ParseVAlignment3;
    NextToken;
    ParseTableRows(CurFoot,
                   EndTokens + [IpHtmlTagTBODY, IpHtmlTagTFOOTend] -
                               [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD]);
    if CurToken = IpHtmlTagTFOOTend then
      NextToken;
  end;
  while not (CurToken in EndTokens) do begin
    case CurToken of
    IpHtmlTagTBODY :
      begin
        CurBody := TIpHtmlNodeTBODY.Create(Parent);
        CurBody.ParseBaseProps(Self);
        CurBody.Align := ParseCellAlign(haLeft);
        CurBody.VAlign := ParseVAlignment3;
        NextToken;
        ParseTableRows(CurBody,
                       EndTokens + [IpHtmlTagTBODYend] -
                         [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD, IpHtmlTagTRend]);
        if CurToken = IpHtmlTagTBODYend then
          NextToken;
      end;
    IpHtmlTagTR :
      begin
        CurBody := TIpHtmlNodeTBODY.Create(Parent);
        ParseTableRows(CurBody,
                       EndTokens - [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD]);
      end;
    else
      exit;
    end;
  end;
end;

procedure TIpHtml.ParseColGroup(Parent: TIpHtmlNode);
var
  CurColGroup : TIpHtmlNodeCOLGROUP;
  CurCol : TIpHtmlNodeCOL;
begin
  while CurToken = IpHtmlTagCOLGROUP do begin
    CurColGroup := TIpHtmlNodeCOLGROUP.Create(Parent);
    with CurColGroup do begin
      ParseBaseProps(Self);
      Span := ParseInteger('SPAN', 1);
      Width := ParseHyperMultiLength('WIDTH', '');
    end;
    NextToken;
    SkipTextTokens;                                                    {!!.10}     
    while CurToken = IpHtmlTagCOL do begin
      CurCol := TIpHtmlNodeCOL.Create(CurColGroup);
      with CurCol do begin
        ParseBaseProps(Self);
        Span := ParseInteger('SPAN', 1);
        Width := ParseHyperMultiLength('WIDTH', '');
      end;
      NextToken;
      SkipTextTokens;
    end;
    if CurToken = IpHtmlTagCOLGROUPend then
      NextToken;
  end;
end;

procedure TIpHtml.ParseTABLE(Parent: TIpHtmlNode;
        const EndTokens: TIpHtmlTokenSet);
var
  CurTable : TIpHtmlNodeTABLE;
  CurCaption : TIpHtmlNodeCAPTION;
begin
  CurTable := TIpHtmlNodeTABLE.Create(Parent);
  with CurTable do begin
    Align := ParseImageAlignment('BOTTOM');
    Width := ParseHyperLength('WIDTH', '');
    Width.OnChange := WidthChanged;                                    {!!.10}
    Border := ParseInteger('BORDER', 0);
    if Border = 0 then begin
      Frame := hfVoid;
      Rules := hrNone;
    end else begin
      Frame := hfBorder;
      Rules := hrAll;
    end;
    CellSpacing := ParseInteger('CELLSPACING', 2);
    CellPadding := ParseInteger('CELLPADDING', 2);
    ParseBaseProps(Self);
    Summary := FindAttribute('SUMMARY');
    Frame := ParseFrameProp(Frame);
    Rules := ParseRules(Rules);
    BgColor := ColorFromString(FindAttribute('BGCOLOR'));
  end;

  repeat
    NextToken;
  until CurToken in
     [IpHtmlTagCAPTION, IpHtmlTagCOLGROUP, IpHtmlTagTHEAD, IpHtmlTagTFOOT,
      IpHtmlTagTBODY, IpHtmlTagTR, IpHtmlTagTABLEend, IpHtmlTagEOF];

  if CurToken = IpHtmlTagCAPTION then begin
    CurCaption := TIpHtmlNodeCAPTION.Create(CurTable);
    CurCaption.Align := ParseVAlignment2;
    CurCaption.ParseBaseProps(Self);
    ParseBodyText(CurCaption,
                  [IpHtmlTagCAPTIONend, IpHtmlTagTABLEend, IpHtmlTagTBODY]);
    if CurToken in EndTokens then
    else
    if CurToken = IpHtmlTagCAPTIONend then
      NextToken
    else
      if FlagErrors then
        ReportExpectedToken(IpHtmlTagCAPTIONend)
      else begin
        while not (CurToken in EndTokens + [IpHtmlTagCAPTIONend]) do
          NextToken;
        if CurToken = IpHtmlTagCAPTIONend then
          NextToken;
      end;
  end;
  ParseColgroup(CurTable);
  SkipTextTokens;                                                      {!!.10}
  ParseTableBody(CurTable, EndTokens + [IpHtmlTagTABLEend]
    - [IpHtmlTagTR, IpHtmlTagP, IpHtmlTagPend, IpHTMLTagCENTERend,
       IpHtmlTagLEFTend, IpHtmlTagRIGHTend, IpHtmlTagBLINKend, IpHtmlTagBLOCKQUOTEend
      ]);
  SkipTextTokens;
  EnsureClosure(IpHtmlTagTABLEend, EndTokens);
end;

procedure TIpHtml.ParseMAP(Parent: TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  CurMap : TIpHtmlNodeMAP;
begin
  CurMap := TIpHtmlNodeMAP.Create(Parent);
  CurMap.Name := FindAttribute('NAME');
  CurMap.ParseBaseProps(Self);
  NextToken;
  while not (CurToken in EndTokens + [IpHtmlTagMAPend]) do begin
    case CurToken of
    IpHtmlTagAREA :
      begin
        with TIpHtmlNodeAREA.Create(CurMap) do begin
          Shape := ParseShape;
          Coords := FindAttribute('COORDS');
          HRef := FindAttribute('HREF');
          NoHRef := ParseBoolean('NOHREF');
          Alt := FindAttribute('ALT');
          TabIndex := ParseInteger('TABINDEX', -1);
          Target := FindAttribute('TARGET');
          ParseBaseProps(Self);
        end;
        NextToken;
      end;
    else
      if FlagErrors then
        ReportExpectedError('</MAP> or <AREA>')
      else
        NextToken;
    end;
  end;
  EnsureClosure(IpHtmlTagMAPend, EndTokens);
end;

procedure TIpHtml.ParseBasefont(Parent : TIpHtmlNode);
var
  CurBasefont : TIpHtmlNodeBASEFONT;
begin
  CurBasefont := TIpHtmlNodeBASEFONT.Create(Parent);
  if CurBasefont=nil then ;
  CurBasefont.Size := ParseInteger('SIZE', 3);
  NextToken;
end;

procedure TIpHtml.ParseInline(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
begin
  case CurToken of
  IpHtmlTagP : ParseParagraph(Parent, EndTokens); {moved from block}  {!!.10}
  IpHtmlTagFont : ParseFont(Parent, EndTokens);
  IpHtmlTagDIV : ParseDiv(Parent, EndTokens);
  IpHtmlTagSPAN : ParseSpan(Parent, EndTokens);
  IpHtmlTagLEFT : ParseLeft(Parent, EndTokens);
  IpHtmlTagCENTER : ParseCenter(Parent, EndTokens);
  IpHtmlTagRIGHT : ParseRight(Parent, EndTokens);
  IpHtmlTagBLINK : ParseBlink(Parent, EndTokens);
  IpHtmlTagQ : ParseQ(Parent, EndTokens);
  IpHtmlTagHR : ParseHR(Parent);
  IpHtmlTagTT, IpHtmlTagI, IpHtmlTagB, IpHtmlTagU, IpHtmlTagSTRIKE, IpHtmlTagS,
  IpHtmlTagBIG, IpHtmlTagSMALL, IpHtmlTagSUB, IpHtmlTagSUP :
    ParseFontStyle(Parent, CurToken, EndTokens + [succ(CurToken)]);
  IpHtmlTagEM, IpHtmlTagSTRONG, IpHtmlTagDFN, IpHtmlTagCODE,
  IpHtmlTagSAMP, IpHtmlTagKBD, IpHtmlTagVAR, IpHtmlTagCITE,
  IpHtmlTagABBR, IpHtmlTagACRONYM :
    ParsePhraseElement(Parent, CurToken, succ(CurToken), EndTokens);
  IpHtmlTagA : ParseAnchor(Parent, EndTokens);
  IpHtmlTagBASEFONT : ParseBasefont(Parent);
  IpHtmlTagBR : ParseBR(Parent);
  IpHtmlTagNOBR : ParseNOBR(Parent);
  IpHtmlTagMAP :
    ParseMAP(Parent, EndTokens);
  IpHtmlTagText :
    begin
      TIpHtmlNodeText.Create(Parent).FEscapedText := GetTokenString;
      NextToken;
    end;
  IpHtmlTagINPUT,
  IpHtmlTagSELECT,
  IpHtmlTagButton,
  IpHtmlTagTEXTAREA :
    ParseFormFields(Parent, EndTokens);
  IpHtmlTagINS :
    ParseIns(Parent, EndTokens);
  IpHtmlTagDEL :
    ParseDel(Parent, EndTokens);
  IpHtmlTagIFRAME :
    ParseIFRAME(Parent);
  IpHtmlTagSCRIPT :
    ParseScript(Parent, EndTokens);
  IpHtmlTagNOSCRIPT :
    ParseNoscript(Parent);
  IpHtmlTagSTYLE :
    ParseStyle(Parent);
  else
    NextToken;
  end;
end;

procedure TIpHtml.ParseBlock(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
begin
  case CurToken of
  IpHtmlTagH1 : ParseHeader(Parent,  IpHtmlTagH1end, 1);
  IpHtmlTagH2 : ParseHeader(Parent, IpHtmlTagH2end, 2);
  IpHtmlTagH3 : ParseHeader(Parent, IpHtmlTagH3end, 3);
  IpHtmlTagH4 : ParseHeader(Parent, IpHtmlTagH4end, 4);
  IpHtmlTagH5 : ParseHeader(Parent, IpHtmlTagH5end, 5);
  IpHtmlTagH6 : ParseHeader(Parent, IpHtmlTagH6end, 6);
  {IpHtmlTagP : ParseParagraph(Parent, EndTokens);} {moved to inline}  {!!.10}
  IpHtmlTagDIR : ParseUnorderedList(Parent, IpHtmlTagDIRend, EndTokens);
  IpHtmlTagMENU : ParseUnorderedList(Parent, IpHtmlTagMENUend, EndTokens);
  IpHtmlTagUL : ParseUnorderedList(Parent, IpHtmlTagULen, EndTokens);
  IpHtmlTagDL : ParseDefinitionList(Parent, EndTokens);
  IpHtmlTagOL :
    ParseOrderedList(Parent,  EndTokens);
  IpHtmlTagPRE : ParsePre(Parent, EndTokens);
  IpHtmlTagBLOCKQUOTE : ParseBlockQuote(Parent, EndTokens);
  IpHtmlTagFORM : ParseForm(Parent, EndTokens);
  IpHtmlTagTABLE : ParseTable(Parent, EndTokens);
  IpHtmlTagIMG : ParseIMG(Parent);
  IpHtmlTagOBJECT : ParseObject(Parent);
  IpHtmlTagAPPLET : ParseApplet(Parent, EndTokens);
  IpHtmlTagADDRESS : ParseAddress(Parent);
  IpHtmlTagEof : exit;
  IpHtmlTagFRAMESET :
    ParseFrameSet(Parent, EndTokens + [IpHtmlTagFRAMESETend]);
  IpHtmlTagUnknown :
    if FlagErrors then
      ReportError(SHtmlUnknownTok)
    else
      NextToken;
  end;
end;

procedure TIpHtml.ParseBodyText(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
begin
  inc(InBlock);
  try
    while not (CurToken in EndTokens) do begin
      case CurToken of
      IpHtmlTagH1,
      IpHtmlTagH2,
      IpHtmlTagH3,
      IpHtmlTagH4,
      IpHtmlTagH5,
      IpHtmlTagH6,
      {IpHtmlTagP,}                                                    {!!.10}
      IpHtmlTagDIR,
      IpHtmlTagMENU,
      IpHtmlTagUL,
      IpHtmlTagDL,
      IpHtmlTagOL,
      IpHtmlTagPRE,
      IpHtmlTagBLOCKQUOTE,
      IpHtmlTagFORM,
      IpHtmlTagTABLE,
      IpHtmlTagIMG,
      IpHtmlTagOBJECT,
      IpHtmlTagAPPLET,
      IpHtmlTagADDRESS,
      IpHtmlTagFRAMESET :
        ParseBlock(Parent, EndTokens);
{Begin !!.12}
      IpHtmlTagBODY :
        begin
          if Body = nil then begin
            TIpHtmlNodeBODY.Create(Parent);
            NextToken;
            ParseBodyText(Body, EndTokens);
          end
          else
            ParseInline(Parent, EndTokens);
        end;
{End !!.12}
      IpHtmlTagEof :
        exit;
      else
        ParseInline(Parent, EndTokens);
      end;
    end;
  finally
    dec(InBlock);
  end;
end;

function TIpHtml.FindAttribute(const AttrName : string) : string;
var
  i : Integer;
begin
  for i := 0 to pred(ParmList.Count) do
    if ParmList[i] = AttrName then begin
      Result := ValueList[i];
      exit;
    end;
  Result := '';
end;

function TIpHtml.ParseInteger(const AttrName : string; Default : Integer) : Integer;
var
  S : string;
  Err : Integer;
begin
  S := FindAttribute(AttrName);
  if (S = '') then
    Result := Default
  else
  if CompareText(S, AttrName) = 0 then
    Result := 1
  else begin
    Val(S, Result, Err);
    if Err <> 0 then begin
      Result := Default;
      if FlagErrors then
        ReportError(SHtmlInvInt)
    end;
  end;
end;

{!!.10 new}
function TIpHtml.ParseHtmlInteger(const AttrName : string; Default : Integer) : TIpHtmlInteger;
var
  S : string;
  N, Err : Integer;
begin
  S := FindAttribute(AttrName);
  if (S = '') then
    N := Default
  else
  if CompareText(S, AttrName) = 0 then
    N := 1
  else begin
    Val(S, N, Err);
    if Err <> 0 then begin
      N := Default;
      if FlagErrors then
        ReportError(SHtmlInvInt)
    end;
  end;
  Result := TIpHtmlInteger.Create(N);
end;

function TIpHtml.ParseRelSize{(const Default : string)} : TIpHtmlRelSize; {!!.10}
var
  S : string;
  Err : Integer;
begin
  Result := TIpHtmlRelSize.Create;                                     {!!.10}
  Result.FSizeType := hrsUnspecified;                                  {!!.10}
  S := FindAttribute('SIZE');
  if (S = '') then
    exit; {S := Default;}                                              {!!.10}
  Result.Value := 0;
  if (length(S) > 1) and (S[1] = '+') then begin
    Result.SizeType := hrsRelative;
    Delete(S,1,1);
  end else
  if (length(S) > 1) and (S[1] = '-') then begin
    Result.SizeType := hrsRelative;
  end else
    Result.SizeType := hrsAbsolute;
  Val(S, Result.FValue, Err);
  if Err <> 0 then
    if FlagErrors then
      ReportError(SHtmlInvInt);
end;

{!!.10 new}
function TIpHtml.ParsePixels(const AttrName: string;
      const Default: string): TIpHtmlPixels;
var
  S : string;
  Err : Integer;
begin
  Result := TIpHtmlPixels.Create;
  S := FindAttribute(AttrName);
  if (S = '') then
    S := Default;
  if S = '' then                                                       {!!.12}
    Result.PixelsType := hpUndefined                                   {!!.12}
  else begin
    Result.PixelsType := hpAbsolute;                                   {!!.12}
    val(S, Result.FValue, Err);
    if (Err <> 0) or (Result.FValue < 0) then begin
      if FlagErrors then
        ReportError(SHtmlInvInt)
      else
        Result.FValue := 0;
    end;
  end;
end;

function TIpHtml.ParseHyperLength(const AttrName: string;
      const Default: string): TIpHtmlLength;
var
  S : string;
  P, Err : Integer;
begin
  Result := TIpHtmlLength.Create;                                      {!!.10}
  Result.LengthType := hlUndefined;
  S := FindAttribute(AttrName);
  if (S = '') then
    S := Default;
  if (S = '') then
    exit;
  P := CharPos('%', S);
  if P <> 0 then begin
    Result.LengthType := hlPercent;
    Delete(S, P, 1);
  end else
    Result.LengthType := hlAbsolute;
  val(S, Result.FLengthValue, Err);                                    {!!.10}
  if (Err <> 0) or (Result.LengthValue < 0) then begin
    if FlagErrors then
      ReportError(SHtmlInvInt)
    else
      Result.LengthType := hlUndefined;
  end else                                                             {!!.12}
    if (Result.LengthType = hlPercent)                                 {!!.12}
    and (Result.LengthValue > 100) then                                {!!.12}
      Result.LengthValue := 100;                                       {!!.12}
end;

function TIpHtml.ParseHyperMultiLength(const AttrName: string;
      const Default: string): TIpHtmlMultiLength;
var
  S : string;
  P, Err : Integer;
begin
  Result := TIpHtmlMultiLength.Create;
  Result.LengthType := hmlUndefined;
  S := FindAttribute(AttrName);
  if (S = '') then
    S := Default;
  if (S = '') then
    exit;
  P := CharPos('%', S);
  if P <> 0 then begin
    Result.LengthType := hmlPercent;
    Delete(S, P, 1);
  end else begin
    P := CharPos('*', S);
    if P <> 0 then begin
      Result.LengthType := hmlRelative;
      Delete(S, P, 1);
    end else
      Result.LengthType := hmlAbsolute;
  end;
  val(S, Result.FLengthValue, Err);
  if (Err <> 0) or (Result.FLengthValue < 0) then begin
    if FlagErrors then
      ReportError(SHtmlInvInt)
    else
      Result.LengthType := hmlUndefined;
  end;
end;

function TIpHtml.ParseHyperMultiLengthList(const AttrName: string;
      const Default: string): TIpHtmlMultiLengthList;                  {!!.10}
var
  S, S2 : string;
  B, E, P, Err : Integer;
  NewEntry: TIpHtmlMultiLength;
begin
  {List.Entries := 0;}
  Result := TIpHtmlMultiLengthList.Create;
  S := FindAttribute(AttrName);
  if (S = '') then
    S := Default;
  if (S = '') then
    exit;
  B := 1;
  while B <= length(S) do begin
    E := B;
    repeat
      inc(E);
    until (E > length(S)) or (S[E] = ',');
    S2 := copy(S, B, E - B);
    NewEntry := TIpHtmlMultiLength.Create;                             {!!.10}
    {List.Values[List.Entries].LengthType := hmlUndefined;}            {!!.10}
    NewEntry.LengthType := hmlUndefined;                               {!!.10}
    P := CharPos('%', S2);
    if P <> 0 then begin
      {List.Values[List.Entries].LengthType := hmlPercent;}            {!!.10}
      NewEntry.LengthType := hmlPercent;                               {!!.10}
      Delete(S2, P, 1);
    end else begin
      P := CharPos('*', S2);
      if P <> 0 then begin
        {List.Values[List.Entries].LengthType := hmlRelative;}         {!!.10}
        NewEntry.LengthType := hmlRelative;                            {!!.10}
        Delete(S2, P, 1);
      end else
        {List.Values[List.Entries].LengthType := hmlAbsolute;}         {!!.10}
        NewEntry.LengthType := hmlAbsolute;                            {!!.10}
    end;
    if S2 = '' then
      {List.Values[List.Entries].LengthValue := 0}                     {!!.10}
      NewEntry.LengthValue := 0                                        {!!.10}
    else begin
      {val(S2, List.Values[List.Entries].FLengthValue, Err);}          {!!.10}
      val(S2, NewEntry.FLengthValue, Err);                             {!!.10}
      {if (Err <> 0) or (List.Values[List.Entries].LengthValue < 0) then begin}
      if (Err <> 0) or (NewEntry.FLengthValue < 0) then begin          {!!.10}
        if FlagErrors then
          ReportError(SHtmlInvInt)
        else
          {List.Values[List.Entries].LengthType := hmlUndefined;}      {!!.10}
          NewEntry.LengthType := hmlUndefined;                         {!!.10}
      end;
    end;
    {inc(List.Entries);}                                               {!!.10}
    Result.AddEntry(NewEntry);
    B := E + 1;
  end;
end;

function CalcMultiLength(const List: TIpHtmlMultiLengthList;
  Avail: Integer; var Sections: Integer): TIntArr; {!!.10}
var
  OrgAvail, i, S : Integer;
begin
  Result := TIntArr.Create;
  if List.Entries = 0 then begin
    Sections := 1;
    Result[0] := Avail;
    exit;
  end;
  OrgAvail := Avail;
  Sections := List.Entries;
  for i := 0 to pred(List.Entries) do begin
    if List.Values[i].LengthType = hmlAbsolute then begin
      if Avail >= List.Values[i].LengthValue then begin
        Result[i] := List.Values[i].LengthValue;
        dec(Avail, Result[i]);
      end else begin
        Result[i] := Avail;
        Avail := 0;
      end;
    end else
      Result[i] := 0;
  end;
  if Avail > 0 then begin
    for i := 0 to pred(List.Entries) do
      if List.Values[i].LengthType = hmlPercent then
        Result[i] := round(List.Values[i].LengthValue * Avail / 100);
    for i := 0 to pred(List.Entries) do
      if List.Values[i].LengthType = hmlPercent then
        dec(Avail, Result[i]);
    if Avail > 0 then begin
      S := 0;
      for i := 0 to pred(List.Entries) do
        if (List.Values[i].LengthType = hmlRelative) then
          inc(S, List.Values[i].LengthValue);
      if S > 0 then
        for i := 0 to pred(List.Entries) do
          if (List.Values[i].LengthType = hmlRelative) then begin
            Result[i] := round(List.Values[i].LengthValue * Avail / S);
            dec(Avail, Result[i]);
          end;
      if Avail > 0 then
        for i := 0 to pred(List.Entries) do
          if (List.Values[i].LengthType = hmlRelative)
          and (List.Values[i].LengthValue = 0) then begin
            Result[i] := Avail;
            break;
          end;
    end;
  end;
  repeat
    S := 0;
    for i := 0 to pred(List.Entries) do
      inc(S, Result[i]);
    S := OrgAvail - S;
    if S > 0 then
      for i := 0 to pred(List.Entries) do begin
        {inc(Result[i]);}                                             {!!.10}
        Result[i] := Result[i] + 1;                                  {!!.10}
        dec(S);
        if S = 0 then break;
      end;
    if S < 0 then
      for i := 0 to pred(List.Entries) do begin
        {dec(Result[i]);}                                             {!!.10}
        Result[i] := Result[i] - 1;                                  {!!.10}
        inc(S);
        if S = 0 then break;
      end;
  until S = 0;
end;

function TIpHtml.ParseBoolean(const AttrName : string) : Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(ParmList.Count) do
    if ParmList[i] = AttrName then begin
      Result := True;
      exit;
    end;
  Result := False;
end;

function TIpHtml.ParseOLStyle(Default : TIpHtmlOLStyle) : TIpHtmlOLStyle;
var
  S : string;
begin
  Result := Default;
  S := FindAttribute('TYPE');
  if (S = '') then
  else
  if (S = '1') then
    Result := olArabic
  else
  if S = 'a' then
    Result := olLowerAlpha
  else
  if S = 'A' then
    Result := olUpperAlpha
  else
  if S = 'i' then
    Result := olLowerRoman
  else
  if S = 'I' then
    Result := olUpperRoman
  else
    if FlagErrors then
      ReportError(SHtmlInvType);
end;

function TIpHtml.ParseULStyle(Default : TIpHtmlULType) : TIpHtmlULType;
var
  S : string;
begin
  Result := Default;
  S := UpperCase(FindAttribute('TYPE'));
  if (S = '') then
  else
  if (S = 'DISC') then
    Result := ulDisc
  else
  if S = 'SQUARE' then
    Result := ulSquare
  else
  if S = 'CIRCLE' then
    Result := ulCircle
  else
    if FlagErrors then
      ReportError(SHtmlInvType);
end;

function TIpHtml.ParseAlignment : TIpHtmlAlign;
var
  S : string;
begin
  Result := haLeft;
  S := UpperCase(FindAttribute('ALIGN'));
  if (S = '') then
    Result := haDefault
  else
  if (S = 'LEFT') then
    Result := haLeft
  else
  if (S = 'CENTER') or (S = 'MIDDLE') then
    Result := haCenter
  else
  if S = 'RIGHT' then
    Result := haRight
  else
  if S = 'JUSTIFY' then
    Result := haJustify
  else
    if FlagErrors then
      ReportError(SHtmlInvAlign);
end;

function TIpHtml.ParseVAlignment : TIpHtmlVAlign;
var
  S : string;
begin
  Result := hvaMiddle;
  S := UpperCase(FindAttribute('VALIGN'));
  if (S = '') or (S = 'MIDDLE') or (S = 'CENTER') then
  else
  if S = 'TOP' then
    Result := hvaTop
  else
  if S = 'BOTTOM' then
    Result := hvaBottom
  else
    if FlagErrors then
      ReportError(SHtmlInvAlign);
end;

function TIpHtml.ParseVAlignment2: TIpHtmlVAlignment2;
var
  S : string;
begin
  Result := hva2Top;
  S := UpperCase(FindAttribute('ALIGN'));
  if (S = '') or (S = 'TOP') then
  else
  if S = 'BOTTOM' then
    Result := hva2Bottom
  else
  if S = 'LEFT' then
    Result := hva2Left
  else
  if S = 'RIGHT' then
    Result := hva2Right
  else
    if FlagErrors then
      ReportError(SHtmlInvAlign);
end;

function TIpHtml.ParseImageAlignment(const Default: string) : TIpHtmlImageAlign;
var
  S : string;
begin
  Result := hiaBottom;
  S := UpperCase(FindAttribute('ALIGN'));
  if S = '' then
    S := Default;
  if (S = 'BOTTOM') then
  else
  if S = 'TOP' then
    Result := hiaTop
  else
  if (S = 'MIDDLE')
  or (S = 'ABSCENTER') then
    Result := hiaMiddle
  else
  if S = 'LEFT' then
    Result := hiaLeft
  else
  if S = 'CENTER' then
    Result := hiaCenter
  else
  if S = 'RIGHT' then
    Result := hiaRight
  else
    if FlagErrors then
      ReportError(SHtmlInvAlign);
end;

function TIpHtml.ParseObjectValueType: TIpHtmlObjectValueType;
var
  S : string;
begin
  Result := hovtData;
  S := UpperCase(FindAttribute('VALUETYPE'));
  if (S = '') or (S = 'DATA') then
  else
  if S = 'REF' then
    Result := hovtRef
  else
  if S = 'OBJECT' then
    Result := hovtObject
  else
    if FlagErrors then
      ReportError(SHtmlInvValType);
end;

function TIpHtml.ParseShape : TIpHtmlMapShape;
var
  S : string;
begin
  Result := hmsDefault;
  S := UpperCase(FindAttribute('SHAPE'));
  if (S = '') or (S = 'DEFAULT') then
  else
  if (S = 'RECT') then
    Result := hmsRect
  else
  if S = 'CIRCLE' then
    Result := hmsCircle
  else
  if (S = 'POLY') or (S = 'POLYGON') then
    Result := hmsPoly
  else
    if FlagErrors then
      ReportError(SHtmlInvShape);
end;

function TIpHtml.ParseMethod : TIpHtmlFormMethod;
var
  S : string;
begin
  Result := hfmGet;
  S := UpperCase(FindAttribute('METHOD'));
  if (S = '') or (S = 'GET') then
  else
  if S = 'POST' then
    Result := hfmPost
  else
    if FlagErrors then
      ReportError(SHtmlInvMethod);
end;

function TIpHtml.ParseBRClear : TIpHtmlBreakClear;
var
  S : string;
begin
  Result := hbcNone;
  S := UpperCase(FindAttribute('CLEAR'));
  if (S = '') then
  else
  if (S = 'ALL') or (S = 'CLEAR') then
    Result := hbcAll
  else
  if S = 'LEFT' then
    Result := hbcLeft
  else
  if S = 'RIGHT' then
    Result := hbcRight
  else
    if FlagErrors then
      ReportError(SHtmlInvAlign);
end;

function TIpHtml.ParseDir : TIpHtmlDirection;
var
  S : string;
begin
  Result := hdLTR;
  S := UpperCase(FindAttribute('DIR'));
  if (S = '') or (S = 'LTR') then
  else
  if (S = 'RTL') then
    Result := hdRTL
  else
    if FlagErrors then
      ReportError(SHtmlInvDir);
end;

function TIpHtml.ColorFromString(S : string) : TColorRef;
var
  R, G, B, Err : Integer;
begin
  Result := -1;
  if S = '' then
    exit;
  S := UpperCase(S);
  if S[1] = '#' then
    if length(S) <> 7 then
      if FlagErrors then
        ReportError(SHtmlInvColor + S)
      else
    else begin
      val('$'+Copy(S,2,2), R, Err);
      if Err <> 0 then
        R := 255;
      val('$'+Copy(S,4,2), G, Err);
      if Err <> 0 then
        G := 255;
      val('$'+Copy(S,6,2), B, Err);
      if Err <> 0 then
        B := 255;
      Result := RGB(R, G, B);
    end
  else
  if S = 'BLACK' then
    Result := clBlack
  else
  if S = 'SILVER' then
    Result := clSilver
  else
  if S = 'GRAY' then
    Result := clGray
  else
  if S = 'WHITE' then
    Result := clWhite
  else
  if S = 'MAROON' then
    Result := clMaroon
  else
  if S = 'RED' then
    Result := clRed
  else
  if S = 'PURPLE' then
    Result := clPurple
  else
  if S = 'FUCHSIA' then
    Result := clFuchsia
  else
  if S = 'GREEN' then
    Result := clGreen
  else
  if S = 'LIME' then
    Result := clLime
  else
  if S = 'OLIVE' then
    Result := clOlive
  else
  if S = 'YELLOW' then
    Result := clYellow
  else
  if S = 'NAVY' then
    Result := clNavy
  else
  if S = 'BLUE' then
    Result := clBlue
  else
  if S = 'TEAL' then
    Result := clTeal
  else
  if S = 'AQUA' then
    Result := clAqua
  else
  if length(S) = 6 then
    try
      val('$'+Copy(S,1,2), R, Err);
      if Err <> 0 then
        R := 255;
      val('$'+Copy(S,3,2), G, Err);
      if Err <> 0 then
        G := 255;
      val('$'+Copy(S,5,2), B, Err);
      if Err <> 0 then
        B := 255;
      Result := RGB(R, G, B);
    except
      if FlagErrors then
        ReportError(SHtmlInvColor + S)
      else
        Result := -1;
    end;
end;

procedure TIpHtml.ParseFrame(Parent : TIpHtmlNode);
var
  CurFrame : TIpHtmlNodeFRAME;
begin
  CurFrame := TIpHtmlNodeFRAME.Create(Parent);
  with CurFrame do begin
    LongDesc := FindAttribute('LONGDESC');
    Name := FindAttribute('NAME');
    Src := FindAttribute('SRC');
    FrameBorder := ParseInteger('BORDER', 1);
    MarginWidth := ParseInteger('MARGINWIDTH', 1);
    MarginHeight := ParseInteger('MARGINHEIGHT', 1);
    NoResize := ParseBoolean('NORESIZE');
    Scrolling := ParseFrameScrollingProp;
    ParseBaseProps(Self);
  end;
  NextToken;
end;

procedure TIpHtml.ParseIFrame(Parent : TIpHtmlNode);
var
  CurFrame : TIpHtmlNodeIFRAME;
begin
  CurFrame := TIpHtmlNodeIFRAME.Create(Parent);
  with CurFrame do begin
    LongDesc := FindAttribute('LONGDESC');
    Name := FindAttribute('NAME');
    Src := FindAttribute('SRC');
    FrameBorder := ParseInteger('BORDER', 1);
    MarginWidth := ParseInteger('MARGINWIDTH', 1);
    MarginHeight := ParseInteger('MARGINHEIGHT', 1);
    Scrolling := ParseFrameScrollingProp;
    Align := ParseAlignment;
    Height := ParseHyperLength('HEIGHT', '');
    Height.OnChange := WidthChanged;                                   {!!.10}
    Width := ParseHyperLength('WIDTH', '');
    Width.OnChange := WidthChanged;                                    {!!.10}
    ParseBaseProps(Self);
  end;
  NextToken;
  ParseBodyText(CurFrame, [IpHtmlTagIFRAMEend]);
  if CurToken = IpHtmlTagIFRAMEend then
    NextToken;
end;

procedure TIpHtml.ParseNOFRAMES(Parent : TIpHtmlNode);
var
  CurNoFrames : TIpHtmlNodeNOFRAMES;
begin
  CurNoFrames := TIpHtmlNodeNOFRAMES.Create(Parent);
  NextToken;
  ParseBodyText(CurNoFrames, [IpHtmlTagNOFRAMESend, IpHtmlTagFRAMESETend]);
  if CurToken = IpHtmlTagNOFRAMESend then
    NextToken;
end;

procedure TIpHtml.ParseFrameSet(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
begin
DebugLn('TIpHtml.ParseFrameSet A');
  FHasFrames := True;
  while CurToken = IpHtmlTagFRAMESET do begin
    CurFrameSet := TIpHtmlNodeFRAMESET.Create(Parent);
    with CurFrameSet do begin
      {ParseHyperMultiLengthList('ROWS', '100%', FRows);}              {!!.10}
      FRows := ParseHyperMultiLengthList('ROWS', '100%');              {!!.10}
      {ParseHyperMultiLengthList('COLS', '100%', FCols);}              {!!.10}
      FCols := ParseHyperMultiLengthList('COLS', '100%');              {!!.10}
      Id := FindAttribute('ID');
      ClassId := FindAttribute('CLASS');
      Title := FindAttribute('TITLE');
      Style := FindAttribute('STYLE');
    end;
    NextToken;
    if CurToken = IpHtmlTagFRAMESET then
      ParseFrameSet(CurFrameSet, EndTokens + [IpHtmlTagFRAMESETend]);
    while CurToken = IpHtmlTagFRAME do
      ParseFrame(CurFrameSet);
    if CurToken = IpHtmlTagNOFRAMES then
      ParseNOFRAMES(CurFrameSet);
    if CurToken = IpHtmlTagFRAMESETend then
      NextToken;
  end;
end;

procedure TIpHtml.ParseBody(Parent : TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var                                                                    {!!.12}
  i : Integer;                                                         {!!.12}
  Node : TIpHtmlNode;                                                  {!!.12}
begin
//  while CurToken = IpHtmlTagText do                                  {Deleted !!.12}
//    NextToken;                                                       {Deleted !!.12}
  if CurToken = IpHtmlTagFRAMESET then begin
    ParseFrameSet(Parent, EndTokens);
    exit;
  end;
  {lead token is optional}
  if CurToken = IpHtmlTagBODY then begin
    TIpHtmlNodeBODY.Create(Parent);
    with Body do begin
      BgColor := ColorFromString(FindAttribute('BGCOLOR'));
      Text := ColorFromString(FindAttribute('TEXT'));
      Link := ColorFromString(FindAttribute('LINK'));
      VLink := ColorFromString(FindAttribute('VLINK'));
      ALink := ColorFromString(FindAttribute('ALINK'));
      Background := FindAttribute('BACKGROUND');
      ParseBaseProps(Self);
    end;

    NextToken;
    ParseBodyText(Body, EndTokens + [IpHtmlTagBODYend]);
    EnsureClosure(IpHtmlTagBODYend, EndTokens);
  end else begin
{Begin !!.12}
//    Body := TIpHtmlNodeBODY.Create(Parent);
//    ParseBodyText(Body, EndTokens + [IpHtmlTagBODYend]);             
    ParseBodyText(Parent, EndTokens + [IpHtmlTagBODYend]);
    { Does the HTML include a body node? }
    if not TIpHtmlNodeHtml(Parent).HasBodyNode then
      { No. Create a body node under FHtml. }
      with TIpHtmlNodeHtml(Parent) do begin
        TIpHtmlNodeBODY.Create(Parent);

        { Make each of FHtml's current children the children of the
          Body node. }
        for i := Pred(ChildCount) downto 0 do
          if ChildNode[i] <> Body then begin
            Node := ChildNode[i];
            FChildren.Remove(Node);
            Node.FParentNode := Body;
            Body.FChildren.Insert(0, Node);
          end;
      end;  { with }
{End !!.12}
    if CurToken = IpHtmlTagBODYend then
      NextToken;
  end;
end;

procedure TIpHtml.ParseHtml;
begin
  {lead token is optional}
  if CurToken = IpHtmlTagHtml then begin
    HtmlNode.Version := FindAttribute('VERSION');
    HtmlNode.Lang := FindAttribute('LANG');
    HtmlNode.Dir := ParseDir;
    NextToken;
    ParseHead(HtmlNode); {may not be present}
    ParseBody(HtmlNode, [IpHtmlTagHtmlend, IpHtmlTagEOF]); {may not be present}
    if CurToken in [IpHtmlTagHtmlend, IpHtmlTagEOF] then
    else
      if FlagErrors then
        ReportExpectedToken(IpHtmlTagHtmlend);
    NextToken;
  end else begin
    ParseHead(HtmlNode); {may not be present}
    ParseBody(HtmlNode, [IpHtmlTagEof]); {may not be present}
  end;
end;

procedure TIpHtml.Parse;
begin
  Getmem(TokenStringBuf, 65536);                                       {!!.01}
  try                                                                  {!!.01}
    CharSP := 0;
    ListLevel := 0;
    StartPos := CharStream.Position;
    repeat
      NextToken;
    until CurToken in [IpHtmlTagHtml, IpHtmlTagFRAMESET, IpHtmlTagEOF];
    if CurToken = IpHtmlTagEOF then begin
      CharStream.Position := StartPos;
      CharSP := 0;
      ListLevel := 0;
      repeat
        NextToken;
      until CurToken <> IpHtmlTagText;
    end;
    if CurToken = IpHtmlTagEOF then exit;
    //ParseDocType; {may not be present}
    ParseHtml;
  finally                                                              {!!.01}
    FreeMem(TokenStringBuf);                                           {!!.01}
    TokenStringBuf := nil;                                             {!!.01}
    if ParmBuf <> nil then begin                                       {!!.12}
      FreeMem(ParmBuf);                                                {!!.12}
      ParmBuf := nil;                                                  {!!.12}
      ParmBufSize := 0;                                                {!!.12}
    end;                                                               {!!.12}
  end;                                                                 {!!.01}
end;

constructor TIpHtml.Create;
var
  TmpBitmap : TBitmap;
begin
  inherited Create;
  PropACache := TList.Create;
  PropBCache := TList.Create;
  DummyA := TIpHtmlPropA.Create;
  DummyA.UseCount := 1;
  DummyB := TIpHtmlPropB.Create(Self);
  DummyB.UseCount := 1;
  PropACache.Add(DummyA);
  PropBCache.Add(DummyB);
  ElementPool := TIpHtmlPoolManager.Create(sizeof(TIpHtmlElement), MaxElements);
  SoftLF := BuildStandardEntry(etSoftLF);
  HardLF := BuildStandardEntry(etHardLF);
  HardLFClearLeft := BuildStandardEntry(etClearLeft);
  HardLFClearRight := BuildStandardEntry(etClearRight);
  HardLFClearBoth := BuildStandardEntry(etClearBoth);
  LIndent := BuildStandardEntry(etIndent);
  LOutdent := BuildStandardEntry(etOutdent);
  SoftHyphen := BuildStandardEntry(etSoftHyphen);
  DefaultProps := TIpHtmlProps.Create(Self);
  FHtml := TIpHtmlNodeHtml.Create(nil);
  FHtml.FOwner := Self;
  ParmList := TStringList.Create;
  ValueList := TStringList.Create;
  AnchorList := TList.Create;
  MapList := TList.Create;
  AreaList := TList.Create;
  MapImgList := TList.Create;
  RectList := TList.Create;
  ControlList := TList.Create;
  LinkColor := clBlue;
  VLinkColor := clPurple;
  ALinkColor := clRed;
  {$IFDEF UseGifImageUnit}
  GifImages := TList.Create;
  {$ELSE}
  AnimationFrames := TList.Create;
  {$ENDIF}
  NameList := TStringList.Create;
  DefaultImage := TPicture.Create;
  TmpBitmap := TBitmap.Create;
  try
    {$IFNDEF IP_LAZARUS}
    TmpBitmap.LoadFromResourceName(FindClassHInstance(                      {!!.06}
      TIpHTMLCustomPanel), 'DEFAULTIMAGE');
    DefaultImage.Graphic := TmpBitmap;
    {$ELSE}
    if LazarusResources.Find('DEFAULTIMAGE')<>nil then begin
      TmpBitmap.LoadFromLazarusResource('DEFAULTIMAGE');
      DefaultImage.Graphic := TmpBitmap;
    end;
    {$ENDIF}
  finally
    TmpBitmap.Free;
  end;
  GifQueue := TList.Create;
  FStartSel.x := -1;
  FEndSel.x := -1;
  FixedTypeface := 'Courier New';                                      {!!.10}
end;

function TIpHtml.LinkVisited(const URL : string): Boolean;
begin
  if (length(URL) > 0) and (URL[1] = '#') then
    Result := True
  else
    Result := CheckKnownURL(URL);
end;

{$IFOPT C+}
procedure TIpHtml.CheckImage(Picture: TPicture);
begin
  if Picture <> nil then begin
    if not (Picture is TPicture) then
      raise EIpHtmlException.Create(SHTMLInvPicture);          {!!.02}
    if Picture.Graphic = nil then
      raise EIpHtmlException.Create(SHTMLNoGraphic);           {!!.02}
    if not (Picture.Graphic is TGraphic) then
      raise EIpHtmlException.Create(SHTMLInvGraphic);          {!!.02}
  end;
end;
{$ENDIF}

procedure TIpHtml.DoGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
begin
  if assigned(FOnGetImageX) then
    OnGetImageX(Sender, URL, Picture)
  else
    raise EIpHtmlException.Create(SHTMLNoGetImage);            {!!.02}
  {$IFOPT C+}
  CheckImage(Picture);
  {$ENDIF}
end;

procedure TIpHtml.FinalizeRecs(P: Pointer);
begin
  {$IFDEF IP_LAZARUS}
  with PIpHtmlElement(P)^ do begin
    //ElementType : TElementType;
    AnsiWord:='';
    //IsBlank : Integer;
    //SizeProp: TIpHtmlPropA;
    //Size: TSize;
    //WordRect2 : TRect;
    //Props : TIpHtmlProps;
    //Owner : TIpHtmlNode;
  end;
  {$ELSE}
  Finalize(PIpHtmlElement(P)^);
  {$ENDIF}
end;

destructor TIpHtml.Destroy;
var
  i : Integer;
begin
  {$IFDEF UseGifImageUnit}
  for i := 0 to pred(GifImages.Count) do
    if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
      TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic).PaintStop;
  {$ELSE}
  for i := 0 to pred(AnimationFrames.Count) do
    if TIpHtmlNodeIMG(AnimationFrames[i]).FPicture <> nil then
      TIpAnimatedGraphic(TIpHtmlNodeIMG(AnimationFrames[i]).FPicture.Graphic).
        AggressiveDrawing := False;
  {$ENDIF}
  Destroying := True;
  PaintBufferBitmap.Free;
  ClearGifQueue;
  Clear;
  GifQueue.Free;
  DefaultImage.Free;
  NameList.Free;
  ParmList.Free;
  ValueList.Free;
  FHtml.Free;
  AnchorList.Free;
  MapList.Free;
  AreaList.Free;
  ClearRectList;
  RectList.Free;
  MapImgList.Free;
  ControlList.Free;
  DefaultProps.Free;
  {$IFDEF UseGifImageUnit}
  GifImages.Free;
  {$ELSE}
  AnimationFrames.Free;
  {$ENDIF}
  ElementPool.EnumerateItems(FinalizeRecs);
  ElementPool.Free;
  ClearCache;
  inherited;
end;

function TIpHtml.ParseFrameProp(Default : TIpHtmlFrameProp): TIpHtmlFrameProp;
var
  S : string;
begin
  Result := hfVoid;
  S := UpperCase(FindAttribute('FRAME'));
  if (S = '') then
    Result := Default
  else
  if (S = 'VOID') then
  else
  if (S = 'ABOVE') then
    Result := hfAbove
  else
  if S = 'BELOW' then
    Result := hfBelow
  else
  if S = 'HSIDES' then
    Result := hfHSides
  else
  if S = 'LHS' then
    Result := hfLhs
  else
  if S = 'RHS' then
    Result := hfRhs
  else
  if S = 'VSIDES' then
    Result := hfvSides
  else
  if S = 'BOX' then
    Result := hfBox
  else
  if S = 'BORDER' then
    Result := hfBorder
  else
    if FlagErrors then
      ReportError(SHtmlInvFrame);
end;

function TIpHtml.ParseRules(Default : TIpHtmlRules): TIpHtmlRules;
var
  S : string;
begin
  Result := hrNone;
  S := UpperCase(FindAttribute('RULES'));
  if (S = '') then
    Result := Default
  else
  if (S = 'NONE') then
  else
  if (S = 'GROUPS') then
    Result := hrGroups
  else
  if S = 'ROWS' then
    Result := hrRows
  else
  if S = 'COLS' then
    Result := hrCols
  else
  if S = 'ALL' then
    Result := hrAll
  else
    if FlagErrors then
      ReportError(SHtmlInvRule);
end;

function TIpHtml.ParseCellAlign(Default : TIpHtmlAlign): TIpHtmlAlign;
var
  S : string;
begin
  Result := haCenter;
  S := UpperCase(FindAttribute('ALIGN'));
  if (S = '') then
    Result := Default
  else
  if (S = 'CENTER') or (S = 'MIDDLE') then
  else
  if (S = 'LEFT') then
    Result := haLeft
  else
  if S = 'RIGHT' then
    Result := haRight
  else
  if S = 'JUSTIFY' then
    Result := haJustify
  else
  if S = 'CHAR' then
    Result := haChar
  else
    if FlagErrors then
      ReportError(SHtmlInvAlign);
end;

function TIpHtml.ParseFrameScrollingProp: TIpHtmlFrameScrolling;
var
  S : string;
begin
  Result := hfsAuto;
  S := UpperCase(FindAttribute('SCROLLING'));
  if (S = '') or (S = 'AUTO') then
  else
  if S = 'YES' then
    Result := hfsYes
  else
  if S = 'NO' then
    Result := hfsNo
  else
    if FlagErrors then
      ReportError(SHtmlInvScroll);
end;

function TIpHtml.ParseVAlignment3: TIpHtmlVAlign3;
var
  S : string;
begin
  Result := hva3Middle;
  S := UpperCase(FindAttribute('VALIGN'));
  if (S = '') then
    Result := hva3Default
  else
  if (S = 'MIDDLE') or (S = 'CENTER') then
  else
  if (S = 'TOP') then
    Result := hva3Top
  else
  if S = 'BOTTOM' then
    Result := hva3Bottom
  else
  if S = 'BASELINE' then
    Result := hva3Baseline
  else
    if FlagErrors then
      ReportError(SHtmlInvAlign);
end;

procedure TIpHtml.SetDefaultProps;
begin
  {$IFDEF IP_LAZARUS}
  if FDefaultTypeFace='' then begin
    {$IFDEF MSWindows}
    Defaultprops.FontName := 'Times New Roman';
    {$ELSE}
    Defaultprops.FontName := Graphics.DefFontData.Name
    {$ENDIF}
  end else
    Defaultprops.FontName := FDefaultTypeface;
  {$ELSE}
  Defaultprops.FontName := 'Times New Roman';
  {$ENDIF}
  Defaultprops.FontSize := 12;
  DefaultProps.BaseFontSize := 3;
  Defaultprops.FontBaseline := 0;
  DefaultProps.VAlignment := hva3Baseline;
  Defaultprops.FontStyle := [];
  Defaultprops.Alignment := haLeft;
  DefaultProps.FontColor := TextColor;
  DefaultProps.LinkColor := LinkColor;
  DefaultProps.VLinkColor := VLinkColor;
  DefaultProps.ALinkColor := ALinkColor;
  DefaultProps.BgColor := -1;
  DefaultProps.Preformatted := False;
  DefaultProps.NoBreak := False;
  if Body <> nil then begin
    if Body.Text <> -1 then
      DefaultProps.FontColor := Body.Text;
    if Body.Link <> -1 then
      DefaultProps.LinkColor := Body.Link;
    if Body.VLink <> -1 then
      DefaultProps.VLinkColor := Body.VLink;
    if Body.ALink <> -1 then
      DefaultProps.ALinkColor := Body.ALink;
    if Body.BgColor <> -1 then
      DefaultProps.BgColor := Body.BgColor;
  end;
end;

function TIpHtml.PagePtToScreen(const Pt : TPoint): TPoint;
{-convert coordinates of point passed in to screen coordinates}
begin
  Result := Pt;
  with PageViewRect do begin
    dec(Result.x, Left);
    dec(Result.y, Top);
  end;
  with ClientRect do begin
    inc(Result.x, Left);
    inc(Result.y, Top);
  end;
end;

function TIpHtml.PageRectToScreen(const Rect: TRect;
  var ScreenRect: TRect): Boolean;
{-convert coordinates of rect passed in to screen coordinates and
  return false if entire rect is clipped}
var
  Tmp : TRect;
begin
  if (Rect.Left = 0) and (Rect.Right = 0) and
     (Rect.Top  = 0) and (Rect.Bottom = 0) then begin
    Result := False;
    exit;
  end;
  if not IntersectRect(Tmp, Rect, PageViewRect) then begin
    Result := False;
    exit;
  end;
  ScreenRect := Rect;
  with PageViewRect do
    OffsetRect(ScreenRect, -Left, -Top);
  with ClientRect do
    OffsetRect(ScreenRect, Left, Top);
  if not IntersectRect(Tmp, ScreenRect, ClientRect) then begin
    Result := False;
    exit;
  end;
  Result := True;
end;

{$IFDEF IP_LAZARUS}
function TIpHtml.GetSelectionBlocks(out StartSelIndex,EndSelIndex: integer): boolean;
var
  R : TRect;
  CurBlock: TIpHtmlNodeBlock;
begin
  result := false;

  if not AllSelected
  and ((FStartSel.x < 0) or (FEndSel.x < 0)) then exit;
  

  if not AllSelected then begin
    CurBlock := nil;
    // search blocks that intersect the selection
    // 1.- find first block that intersect upleft  point of sel. (start from 0)
    StartSelIndex := 0;
    while StartSelIndex < RectList.Count do begin
      CurBlock := PIpHtmlRectListEntry(RectList[StartSelIndex]).Block;
      {if AllSelected and (CurBlock <> nil) then
        break;}
      if PtInRect(CurBlock.PageRect, FStartSel) then begin
        R := PIpHtmlRectListEntry(RectList[StartSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          // block within selection (vertically)
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          // selection start or ends in this block
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      inc(StartSelIndex);
    end;
    if StartSelIndex >= RectList.Count then exit;
    // 2.- find first block thta intersect downright point of sel. (start from count-1)
    EndSelIndex := pred(RectList.Count);
    while EndSelIndex >= StartSelIndex do begin
      if PIpHtmlRectListEntry(RectList[EndSelIndex]).Block = CurBlock then begin
        {if AllSelected then
          break;}
        R := PIpHtmlRectListEntry(RectList[EndSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      dec(EndSelIndex);
    end;
  end else begin
    StartSelIndex := 0;
    EndSelIndex := RectList.Count - 1;
  end;
  result := True;
end;
{$ENDIF}

procedure TIpHtml.PaintSelection;
var
  StartSelIndex, EndSelIndex,
  i : Integer;
  R : TRect;
  CurBlock: TIpHtmlNodeBlock;
begin
  if not AllSelected
  and ((FStartSel.x < 0) or (FEndSel.x < 0)) then exit;
  if not AllSelected then begin
    CurBlock := nil;
    StartSelIndex := 0;
    while StartSelIndex < RectList.Count do begin
      CurBlock := PIpHtmlRectListEntry(RectList[StartSelIndex]).Block;
      {if AllSelected and (CurBlock <> nil) then
        break;}
      if PtInRect(CurBlock.PageRect, FStartSel) then begin
        R := PIpHtmlRectListEntry(RectList[StartSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      inc(StartSelIndex);
    end;
    if StartSelIndex >= RectList.Count then exit;
    EndSelIndex := pred(RectList.Count);
    while EndSelIndex >= StartSelIndex do begin
      if PIpHtmlRectListEntry(RectList[EndSelIndex]).Block = CurBlock then begin
        {if AllSelected then
          break;}
        R := PIpHtmlRectListEntry(RectList[EndSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      dec(EndSelIndex);
    end;
  end else begin
    StartSelIndex := 0;
    EndSelIndex := RectList.Count - 1;
  end;
  for i := StartSelIndex to EndSelIndex do begin
    R := PIpHtmlRectListEntry(RectList[i]).Rect;
    if PageRectToScreen(R, R) then begin
      {$IFDEF IP_LAZARUS}
      DebugLn('TIpHtml.PaintSelection  PatBlt not implemented');
      {$ELSE}
      PatBlt(PaintBuffer.Handle, R.Left, R.Top,
        R.Right - R.Left, R.Bottom - R.Top, DSTINVERT);
      {$ENDIF}
    end;
  end;
end;

procedure TIpHtml.RequestImageNodes(Node : TIpHtmlNode);
var
  i : Integer;
begin
  if Node is TIpHtmlNodeIMG then begin
    if TIpHtmlNodeIMG(Node).FPicture = nil then
      TIpHtmlNodeIMG(Node).LoadImage;
  end;
  if Node is TIpHtmlNodeMulti then
    for i := 0 to pred(TIpHtmlNodeMulti(Node).ChildCount) do begin
      RequestImageNodes(TIpHtmlNodeMulti(Node).ChildNode[i]);
    end;
end;

{$IFDEF IP_LAZARUS_DBG}
var
  CCC: Integer;
  
procedure TIpHtml.DebugChild(Node: TIpHtmlNode; const UserData: Pointer);
var
  i: Integer;
begin
  if Node=UserData then
    Write('Parent: ');
  for i:=0 to CCC do Write(' ');
  Write('Node: ', Node.ClassName);
  if Node is TIpHtmlNodeText then
    Write(' ', TIpHtmlNodeText(NodE).ANSIText);
  WriteLn;
  if Node=UserData then
    exit;
  inc(CCC);
  Node.EnumChildren(DebugChild, Node);
  dec(CCC);
end;

procedure TIpHtml.DebugAll;
var
  i: Integer;
  item: PIpHtmlRectListEntry;
  Node: TIpHtmlNode;
begin
  CCC := 0;
  Fhtml.EnumChildren(DebugChild, FHtml);
  {
  
  for i:=0 to RectList.Count-1 do begin
    WriteLn('RectList[',i,']:');
    Item := PIpHtmlRectListEntry(Rectlist[i]);
    if Item<>nil then begin
      WriteLn(' Node=', dbgs(Item.Node));
      WriteLn('   Owner=', dbgs(Item.Node^.Owner));
      WriteLn('    Text=', Item.Node^.AnsiWord);
      Node := Item.Node^.Owner;
      if Node<>nil then begin
      WriteLn('     ClassName:', Node.ClassName);
        if Node is TIpHtmlNodeText then
          WriteLn('       Text=', TIpHtmlNodeText(Node).ANSIText);
      end;
      WriteLn(' Block=', dbgs(Item.Block));
      WriteLn(' Rect=', dbgs(Item.Rect));
    end;
  end;
  }
end;
{$ENDIF}

procedure TIpHtml.Render(TargetCanvas: TCanvas; TargetPageRect : TRect;
  UsePaintBuffer: Boolean; const TopLeft: TPoint);                 {!!.10}
var
  i : Integer;
begin
  ClientRect.TopLeft := TopLeft; {Point(0, 0);}                        {!!.10}
  ClientRect.Right := TargetPageRect.Right - TargetPageRect.Left;
  ClientRect.Bottom := TargetPageRect.Bottom - TargetPageRect.Top;
  if not DoneLoading then begin
    TargetCanvas.FillRect(ClientRect);
    exit;
  end;
  {$IFDEF UseGifImageUnit}
  for i := 0 to pred(GifImages.Count) do
    if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
      with TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic) do
        if Painters <> nil then
          PaintStop;
  {$ELSE}
  for i := 0 to pred(AnimationFrames.Count) do
    if TIpHtmlNodeIMG(AnimationFrames[i]).FPicture <> nil then
      with TIpAnimatedGraphic(TIpHtmlNodeIMG(AnimationFrames[i]).FPicture.Graphic) do
        AggressiveDrawing := False;
  {$ENDIF}
  for i := 0 to pred(ControlList.Count) do
    TIpHtmlNode(ControlList[i]).UnmarkControl;
  SetDefaultProps;
  PageViewRect := TargetPageRect;
  if UsePaintBuffer then begin
    if (PaintBuffer = nil)
    or (PaintBufferBitmap.Width <> Clientrect.Right)
    or (PaintBufferBitmap.Height <> ClientRect.Bottom) then begin
      PaintBufferBitmap.Free;
      PaintBufferBitmap := TBitmap.Create;
      PaintBufferBitmap.Width := ClientRect.Right;
      PaintBufferBitmap.Height := ClientRect.Bottom;
      PaintBuffer := PaintBufferBitmap.Canvas;
    end;
    FTarget := PaintBuffer;
  end else begin
    PaintBuffer := TargetCanvas;
    FTarget := TargetCanvas;
  end;
  ClearRectList;
  if FHtml <> nil then
    FHtml.Render(DefaultProps);

  for i := 0 to pred(ControlList.Count) do
    TIpHtmlNode(ControlList[i]).HideUnmarkedControl;
  {$IFNDEF IP_LAZARUS}
  PaintSelection;
  {$ENDIF}
  if UsePaintBuffer then
    TargetCanvas.CopyRect(ClientRect, PaintBuffer, ClientRect)
  else
    if PaintBufferBitmap <> nil then
      PaintBuffer := PaintBufferBitmap.Canvas
    else
      PaintBuffer := nil;
  StartGifPaint(TargetCanvas);
  {Request all non-visible images}
  RequestImageNodes(HtmlNode);
end;

procedure TIpHtml.ResetElementMetrics(P: Pointer);
begin
  with PIpHtmlElement(P)^ do begin
    Size.cx := 0;
    Size.cy := 0;
    WordRect2 := Rect(0, 0, 0, 0);
    SizeProp := nil;
  end;
end;

procedure TIpHtml.ResetWordLists;
begin
  ElementPool.EnumerateItems(ResetElementMetrics);
end;

procedure TIpHtml.ResetBlocks(Node: TIpHtmlNode);
var
  i : Integer;
begin
  if Node = nil then exit;
  if Node is TIpHtmlNodeBlock then
    with TIpHtmlNodeBlock(Node) do begin
      InvalidateSize;
    end
  else
  if Node is TIpHtmlNodeTable then
    with TIpHtmlNodeTable(Node) do begin
      FMin := -1;
      FMax := -1;
    end;
  if Node is TIpHtmlNodeMulti then
    for i := 0 to pred(TIpHtmlNodeMulti(Node).ChildCount) do
      ResetBlocks(TIpHtmlNodeMulti(Node).ChildNode[i]);
end;

{!!.02}
procedure TIpHtml.ResetImages(Node: TIpHtmlNode);
var
  i : Integer;
begin
  if Node = nil then exit;
  if Node is TIpHtmlNodeIMG then
    with TIpHtmlNodeIMG(Node) do begin
      {UnloadImage;}                                                   {!!.10}
      InvalidateSize;
    end
  else
  if Node is TIpHtmlNodeMulti then
    for i := 0 to pred(TIpHtmlNodeMulti(Node).ChildCount) do
      ResetImages(TIpHtmlNodeMulti(Node).ChildNode[i]);
end;

procedure TIpHtml.ResetCanvasData;
begin
  ResetCache;
  ResetWordLists;
  ResetBlocks(FHtml);
  ResetImages(FHtml);                                                  {!!.02}
end;

function TIpHtml.GetPageRect(TargetCanvas: TCanvas;  Width,
  Height : Integer): TRect;
var
  DefPageRect : TRect;
  Min, Max, W, H : Integer;
begin
  if not DoneLoading then begin
    {$IFDEF IP_LAZARUS}
    // always set result
    SetRectEmpty(Result);
    {$ENDIF}
    exit;
  end;
  DoneLoading := False;
  SetRectEmpty(FPageRect);
  if FHtml <> nil then begin
    if (TargetCanvas <> RenderCanvas)
    or (PageHeight <> Height) then
      ResetCanvasData;
    PageHeight := Height;
    SetDefaultProps;
    {PanelWidth := Width;}                                             {!!.12}
    FTarget := TargetCanvas;
    FHtml.CalcMinMaxWidth(DefaultProps, Min, Max);
    W := MaxI2(Min + 2 * MarginWidth, Width);
    H := FHtml.GetHeight(DefaultProps, W - 2 * MarginWidth) + 2 * MarginHeight;
    DefPageRect := Rect(
      MarginWidth,
      MarginHeight,
      W - MarginWidth,
      H - MarginHeight);
    ClearAreaLists;
    ClearAreaList;
    FHtml.Layout(DefaultProps, DefPageRect);
    FPageRect := DefPageRect;
    FPagerect.Bottom := FPageRect.Bottom + MarginHeight;
    FPageRect.Right := FPageRect.Right + MarginWidth;
    RenderCanvas := TargetCanvas;
  end;
  Result := FPageRect;
  DoneLoading := True;
end;

procedure TIpHtml.InvalidateSize;
begin
  if assigned(FOnInvalidateSize) then
    FOnInvalidateSize(Self);
end;

procedure TIpHtml.ClearAreaList;
var
  i : Integer;
begin
  for i := 0 to pred(AreaList.Count) do
    TIpHtmlNodeArea(AreaList[i]).Reset;
  AreaList.Clear;
end;

function RectFromString(const S: string): TRect;
var
  i, j, x, err : Integer;

  procedure Next;
  begin
    j := i;
    while (j <= length(S)) and (S[j] <> ',') do
      inc(j);
    val(copy(S, i, j - i), x, err);
  end;

begin
  SetRectEmpty(Result);
  i := 1;
  Next;
  if err <> 0 then exit;
  Result.Left := x;
  i := j + 1;
  Next;
  if err <> 0 then exit;
  Result.Top := x;
  i := j + 1;
  Next;
  if err <> 0 then exit;
  Result.Right := x;
  i := j + 1;
  Next;
  if err <> 0 then exit;
  Result.Bottom := x;
end;

function CircularRegion(const Coords: string; const Rect: TRect): HRgn;
var
  i, j, err, cx, cy, R : Integer;
begin
  Result := 0;
  i := 1;
  j := i;
  while (j <= length(Coords)) and (Coords[j] <> ',') do
    inc(j);
  val(copy(Coords, i, j - i), cx, err);
  if err <> 0 then exit;
  i := j + 1;
  j := i;
  while (j <= length(Coords)) and (Coords[j] <> ',') do
    inc(j);
  val(copy(Coords, i, j - i), cy, err);
  if err <> 0 then exit;
  i := j + 1;
  j := i;
  while (j <= length(Coords)) and (Coords[j] <> ',') and (Coords[j] <> '%') do
    inc(j);
  val(copy(Coords, i, j - i), R, err);
  if err <> 0 then exit;
  if (j <= length(Coords)) and (Coords[j] = '%') then
    R := round(R * MinI2(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top) / 100);
  if R < 1 then exit;
  Result := CreateEllipticRgn(
    Rect.Left + cx - R,
    Rect.Top + cy - R,
    Rect.Left + cx + R,
    Rect.Top + cy + R);
end;

function PolygonRegion(const Coords: string; const Rect: TRect): HRgn;
const
  MAXPOINTS = 4096;
var
  Points : array [0.. pred(MAXPOINTS)] of TPoint;
  Count, i, j, x, y, err : Integer;
begin
  Result := 0;
  Count := 0;
  i := 1;
  while i < length(Coords) do begin
    j := i;
    while (j <= length(Coords)) and (Coords[j] <> ',') do
      inc(j);
    val(copy(Coords, i, j - i), x, err);
    if err <> 0 then exit;
    i := j + 1;
    j := i;
    while (j <= length(Coords)) and (Coords[j] <> ',') do
      inc(j);
    val(copy(Coords, i, j - i), y, err);
    if err <> 0 then exit;
    Points[Count].x := x + Rect.Left;
    Points[Count].y := y + Rect.Top;
    inc(Count);
    i := j + 1;
  end;
  if Count < 3 then exit;
  if (Points[0].x <> Points[Count - 1].x)
  or (Points[0].y <> Points[Count - 1].y) then begin
    Points[Count] := Points[0];
    inc(Count);
  end;
  Result := CreatePolygonRgn(
    {$IFDEF IP_LAZARUS}
    PPoint(@Points[0]),
    {$ELSE}
    (@Points[0])^,
    {$ENDIF}
    Count,
    ALTERNATE); {fill mode is irrelevant here}
end;

procedure TIpHtml.BuildAreaList;
var
  i, j, k : Integer;
  R, R2 : TRect;
begin
  ClearAreaList;
  for i := 0 to pred(MapImgList.Count) do
    with TIpHtmlNodeIMG(MapImgList[i]) do begin
      R := GrossDrawRect;
      for j := 0 to pred(MapList.Count) do
        with TIpHtmlNodeMap(MapList[j]) do begin
          for k := 0 to pred(FChildren.Count) do
            if TIpHtmlNode(FChildren[k]) is TIpHtmlNodeArea then begin
              with TIpHtmlNodeArea(FChildren[k]) do begin
                if HRef <> '' then begin
                  case Shape of
                  hmsDefault :
                    FRect := R;
                  hmsRect :
                    begin
                      R2 := RectFromString(Coords);
                      OffsetRect(R2, R.Left, R.Top);
                      FRect := R2;
                    end;
                  hmsCircle :
                    FRgn := CircularRegion(Coords, R);
                  hmsPoly :
                    FRgn := PolygonRegion(Coords, R);
                  end;
                end;
              end;
              AreaList.Add(TIpHtmlNodeArea(FChildren[k]));
            end;
        end;
    end;
end;

procedure TIpHtml.MouseMove(Pt: TPoint);
var
  i : Integer;
begin
  FHotPoint := Point(-1, -1);
  if (MapList.Count > 0) and (AreaList.Count = 0) then
    BuildAreaList;
  for i := 0 to pred(AnchorList.Count) do
    if TIpHtmlNodeA(AnchorList[i]).PtInRects(Pt) then begin
      if FHotNode <> TIpHtmlNodeA(AnchorList[i]) then begin
        if FHotNode <> nil then
          if FHotNode is TIpHtmlNodeA then
           TIpHtmlNodeA(FHotNode).Hot := False;
        FHotNode := TIpHtmlNode(AnchorList[i]);
        if FHotNode is TIpHtmlNodeA then
          TIpHtmlNodeA(FHotNode).Hot := True;
      end;
      if (FHotNode <> nil) then
        if FHotNode is TIpHtmlNodeA then
          FHotPoint := TIpHtmlNodeA(FHotNode).RelMapPoint(Pt);
      exit;
    end;
  for i := 0 to pred(AreaList.Count) do
    if TIpHtmlNodeAREA(AreaList[i]).PtInRects(Pt) then begin
      if FHotNode <> AreaList[i] then begin
        if FHotNode <> nil then
          if FHotNode is TIpHtmlNodeA then
            TIpHtmlNodeA(FHotNode).Hot := False;
        FHotNode := TIpHtmlNode(AreaList[i]);
      end;
      exit;
    end;
  if FHotNode <> nil then
    if FHotNode is TIpHtmlNodeA then
      TIpHtmlNodeA(FHotNode).Hot := False;
  FHotNode := nil;
  FCurElement := nil;
  for i := 0 to pred(RectList.Count) do
    if PtInRect(PIpHtmlRectListEntry(RectList[i]).Rect, Pt) then begin
      FCurElement := PIpHtmlRectListEntry(RectList[i]).Node;
      break;
    end;
end;

function TIpHtml.BuildPath(const Ext: string): string;
begin
  Result := BuildURL(CurURL, Ext);
end;

function TIpHtml.NewElement(EType : TElementType; Own: TIpHtmlNode) : PIpHtmlElement;
begin
  Result := ElementPool.NewItm;
  Result.ElementType := EType;
  Result.Owner := Own;
  {$IFDEF IP_LAZARUS}
  Result.IsSelected := False;
  {$ENDIF}
end;

function TIpHtml.BuildStandardEntry(EType: TElementType): PIpHtmlElement;
begin
  Result := NewElement(EType, nil);
  Result.Props := nil;
  SetWordRect(Result, Rect(0, 0, 0, 0));
end;

procedure TIpHtml.MakeVisible(const R: TRect);
begin
  if assigned(FOnScroll) then
    FOnScroll(Self, R);
end;

function TIpHtml.FindElement(const Name: string): TIpHtmlNode;
var
  i : Integer;
begin
  NameList.Sorted := True;
  i := NameList.IndexOf(Name);
  if i <> -1 then
    Result := TIpHtmlNode(NameList.Objects[i])
  else
    Result := nil;
end;

type
  TIpHtmlGifQueueEntry = class
  protected
    FGraphic : TGraphic;
    FR : TRect;
  public
    constructor Create(AGraphic: TGraphic; ARect: TRect);
    property Graphic : TGraphic read FGraphic;
    property R : TRect read FR;
  end;

procedure TIpHtml.ClearAreaLists;
var
  i : Integer;
begin              
  for i := 0 to pred(AnchorList.Count) do
    TIpHtmlNodeA(AnchorList[i]).ClearAreaList;
end;

procedure TIpHtml.Home;
begin
  MakeVisible(Rect(0, 0, 1, 1));
end;

procedure TIpHtml.Get(const URL: string);
begin
  if assigned(FOnGet) then
    FOnGet(Self, URL);
end;

procedure TIpHtml.Post(const URL: string; FormData: TIpFormDataEntity); {!!.12}
begin
  if assigned(FOnPost) then
    FOnPost(Self, URL, FormData); {!!.12}
end;

procedure TIpHtml.AddRect(const R : TRect; Node : PIpHtmlElement;
  Block: TIpHtmlNodeBlock);
var
  NewEntry : PIpHtmlRectListEntry;
begin
  New(NewEntry);
  NewEntry.Rect := R;
  NewEntry.Node := Node;
  NewEntry.Block := Block;
  RectList.Add(NewEntry);
end;

procedure TIpHtml.ClearRectList;
var
  i : Integer;
  p: PIpHtmlRectListEntry;
begin
  for i := pred(RectList.Count) downto 0 do begin
    p:=PIpHtmlRectListEntry(RectList[i]);
    Freemem(p);
  end;
  RectList.Clear;
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtml.DeselectAllItems(Item: Pointer);
begin
  PIpHtmlElement(item)^.IsSelected := False;
end;
{$ENDIF}

procedure TIpHtml.SetSelection(StartPoint, EndPoint: TPoint);
{$IFDEF IP_LAZARUS}
var
  StartSelIndex,EndSelindex: Integer;
  i: integer;
  r: TRect;
  Selected: boolean;
  DeselectAll: boolean;
  item: PIpHtmlRectListEntry;
{$ENDIF}
begin
  AllSelected := False;
  if EndPoint.y > StartPoint.y then begin
    FStartSel := StartPoint;
    FEndSel := EndPoint;
  end
  else
  if EndPoint.y = StartPoint.y then
    if EndPoint.x > StartPoint.x then begin
      FStartSel := StartPoint;
      FEndSel := EndPoint;
    end else begin
      FStartSel := EndPoint;
      FEndSel := StartPoint;
    end
  else begin
    FStartSel := EndPoint;
    FEndSel := StartPoint;
  end;
  {$IFDEF IP_LAZARUS}
  if Body <> nil then begin
    // invalidate only those blocks that need it
    DeselectAll := (EndPoint.x<0)and(EndPoint.y<0);
    GetSelectionBlocks(StartSelIndex,EndSelIndex);
    for i:= 0 to RectList.Count-1 do begin
      item := PIpHtmlRectListEntry(RectList[i]);
      // (de)select only text elements
      if Item.Node.ElementType<>etWord then
        continue;
      if DeselectAll then
        Selected := false
      else
        Selected := (StartSelIndex<=i)and(i<=EndSelIndex);
      // invalidate only changed elements
      if Item.Node.IsSelected<>Selected then begin
        Item.Node.IsSelected := Selected;
        if Body.PageRectToScreen(Item^.Rect, R) then
          InvalidateRect(R);
      end;
    end;
    // also deselect remaining elements
    if DeselectAll then
      ElementPool.EnumerateItems(DeselectAllItems);
  end;
  {$ELSE}
  if Body <> nil then
    InvalidateRect(Body.PageRect);
  {$ENDIF}
end;

procedure TIpHtml.SelectAll;
begin
  AllSelected := True;
end;

{!!.10 new}
procedure TIpHtml.DeselectAll;
begin
  AllSelected := False;
  FStartSel.x := -1;
  FEndSel.x := -1;
end;

procedure TIpHtml.CopyToClipboard;
var
  S : string;
begin
  if HaveSelection then begin
    S := '';
    if FHtml <> nil then
      FHtml.AppendSelection(S);
    if S <> '' then begin
      Clipboard.Open;
      try
        Clipboard.Clear;
        Clipboard.AsText := S;
      finally
        Clipboard.Close;
      end;
    end;
  end;
end;

function TIpHtml.HaveSelection: Boolean;
begin
  Result := AllSelected or ((FEndSel.x > 0) or (FEndSel.y > 0));
end;

procedure TIpHtml.CreateIFrame(Parent: TWinControl;
  Frame: TIpHtmlNodeIFRAME;
      var Control: TWinControl);
begin
  if assigned(FOnIFrameCreate) then
    FOnIFrameCreate(Self, Parent, Frame, Control);
end;

function TIpHtml.CheckKnownURL(URL: string): boolean;
var
  P : Integer;
begin
  if assigned(FOnURLCheck) then begin
    P := CharPos('#', URL);
    if P <> 0 then
      SetLength(URL, P - 1);
    {$IFDEF IP_LAZARUS}
    Result:=true;
    {$ENDIF}
    FOnURLCheck(Self, URL, Result);
  end;
end;

procedure TIpHtml.ReportReference(URL: string);
var
  P : Integer;
begin
  if assigned(FOnReportURL) then begin
    P := CharPos('#', URL);
    if P <> 0 then
      if P = 1 then
        exit
      else
        SetLength(URL, P - 1);
    FOnReportURL(Self, URL);
  end;
end;

procedure TIpHtml.ControlClick(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlClick) then
    FControlClick(Self, Sender);
end;

procedure TIpHtml.ControlCreate(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlCreate) then
    FControlCreate(Self, Sender);
end;

{ TIpHtmlGifQueueEntry }

constructor TIpHtmlGifQueueEntry.Create(AGraphic: TGraphic; ARect: TRect);
begin
  {$IFDEF IP_LAZARUS}
  DebugLn('TIpHtmlGifQueueEntry.Create ToDo NOT IMPLEMENTED YET');
  {$ELSE}
  FGraphic := AGraphic;
  {$ENDIF}
  FR := ARect;
end;

procedure TIpHtml.AddGifQueue(Graphic: TGraphic; const R: TRect);
begin
  GifQueue.Add(TIpHtmlGifQueueEntry.Create(Graphic, R));
end;

procedure TIpHtml.StartGifPaint(Target: TCanvas);
var
  i : Integer;
begin
  for i := 0 to pred(GifQueue.Count) do
    with TIpHtmlGifQueueEntry(GifQueue[i]) do
      Target.StretchDraw(R, Graphic);
  ClearGifQueue;
end;

procedure TIpHtml.ClearGifQueue;
var
  i : integer;
begin
  if Assigned(GifQueue) then                                           {!!.12}
    for i := pred(GifQueue.Count) downto 0 do begin
      TIpHtmlGifQueueEntry(GifQueue[i]).Free;
      GifQueue.Delete(i);
    end;
end;

{ TIpHtmlNodeText }

procedure TIpHtmlNodeText.SetProps(const RenderProps: TIpHtmlProps);
begin
  PropsR := RenderProps;
end;

procedure TIpHtmlNodeText.Enqueue;
begin
  BuildWordList;
end;

procedure TIpHtmlNodeText.BuildWordList;
var
  NewEntry : PIpHtmlElement;
  l : Integer;
  B, N, N2 : PAnsiChar;
  First : Boolean;
  Ch : AnsiChar;
  ImplicitLF: Boolean;                                                 {!!.10}
begin
  First := True;
  ImplicitLF := False;                                                 {!!.10}
  if PropsR.Preformatted then begin
    l := length(EscapedText);
    if l > 0 then begin
      Getmem(B, l + 1);
      try
        TrimFormattingPre(EscapedText, B);
        N := B;
        while N^ <> #0 do begin
          case N^ of
          CR :                                                         {!!.10}
            ImplicitLF := True;                                        {!!.10}
          LF :
            begin
              EnqueueElement(Owner.HardLF);
              inc(N);
              ImplicitLF := False;                                     {!!.10}
            end;
          else
            begin
              if ImplicitLF then begin                                 {!!.10}
                EnqueueElement(Owner.HardLF);                          {!!.10}
                inc(N);                                                {!!.10}
                ImplicitLF := False;                                   {!!.10}
              end;                                                     {!!.10}
              N2 := StrScan(N, CR);                                    {!!.10}
              if N2 <> nil then begin                                  {!!.10}
                N2^ := #0;                                             {!!.10}
                if First then                                          {!!.10}
                  Owner.AddWord(N, PropsR, Self)                       {!!.10}
                else                                                   {!!.10}
                  Owner.AddWord(N, nil, Self);                         {!!.10}
                N2^ := CR;                                             {!!.10}
                First := False;                                        {!!.10}
                N := N2;                                               {!!.10}
              end else begin
                N2 := StrScan(N, LF);
                if N2 <> nil then begin
                  N2^ := #0;
                  if First then
                    Owner.AddWord(N, PropsR, Self)
                  else
                    Owner.AddWord(N, nil, Self);
                  N2^ := LF;
                  First := False;
                  N := N2;
                end else begin
                  if First then
                    Owner.AddWord(N, PropsR, Self)
                  else
                    Owner.AddWord(N, nil, Self);
                  First := False;
                  N^ := #0;
                end;
              end;
            end;
          end;
        end;
      finally
        FreeMem(B);
      end;
    end;
  end else begin
    l := length(EscapedText);
    if l > 0 then begin
      Getmem(B, l + 1);
      try
        TrimFormattingNormal(EscapedText, B);
        N := B;
        while N^ <> #0 do begin
          case N^ of
          LF :
            begin
              EnqueueElement(Owner.HardLF);
              inc(N);
            end;
          ' ' :
            begin
              if not ElementQueueIsEmpty then begin                    {!!.10}
                NewEntry := Owner.NewElement(etWord, Self);
                NewEntry.AnsiWord := ' ';
                NewEntry.IsBlank := 1;
                if First then
                  NewEntry.Props := PropsR
                else
                  NewEntry.Props := nil;
                EnqueueElement(NewEntry);
                First := False;
              end;                                                     {!!.10}
              inc(N);
            end;
          else
            begin
              N2 := N;
              while not (N2^ in [#0, ' ', LF]) do
                inc(N2);
              if N2^ <> #0 then begin
                Ch := N2^;
                N2^ := #0;
                if First then
                  Owner.AddWord(N, PropsR, Self)
                else
                  Owner.AddWord(N, nil, Self);
                N2^ := Ch;
                First := False;
                N := N2;
              end else begin
                if First then
                  Owner.AddWord(N, PropsR, Self)
                else
                  Owner.AddWord(N, nil, Self);
                First := False;
                N^ := #0;
              end;
            end;
          end;
        end;
      finally
        FreeMem(B);
      end;
    end;
  end;
end;

function TIpHtmlNodeText.GetAnsiText: string;
begin
  Result := EscapeToAnsi(FEscapedText);
end;

procedure TIpHtmlNodeText.EnqueueElement(const Entry: PIpHtmlElement);
begin
  FParentNode.EnqueueElement(Entry);
end;

function FindInnerBlock(Node : TIpHTMLNode): TIpHtmlNodeBlock;
begin
  while not (Node is TIpHtmlNodeBlock) do
    Node := Node.FParentNode;

  Result := TIpHtmlNodeBlock(Node);
end;

procedure TIpHtmlNodeText.SetAnsiText(const Value: string);
begin
  EscapedText := AnsiToEscape(Value);
end;

procedure TIpHtmlNodeText.SetEscapedText(const Value: string);
var
  Block: TIpHtmlNodeBlock;
begin
  FEscapedText := Value;
  Block := FindInnerBlock(Self);

  {we need to clear the queue so that it will be built again}

  Block.ClearWordList;

  {then, we need to invalidate the block so that
   the rendering logic recalculates everything}

  Block.InvalidateSize;
end;

procedure TIpHtmlNodeText.ReportDrawRects(M: TRectMethod);
begin
  ReportCurDrawRects(Self, M);
end;

{!!.10 new}
function TIpHtmlNodeText.ElementQueueIsEmpty: Boolean;
begin
  Result := FParentNode.ElementQueueIsEmpty;
end;

{ TIpHtmlNodeFONT }

procedure TIpHtmlNodeFONT.ApplyProps(const RenderProps: TIpHtmlProps);
var
  TmpSize : Integer;
begin
  Props.Assign(RenderProps);
  if Face <> '' then
    Props.FontName := FirstString(Face);
  case Size.SizeType of
  hrsAbsolute :
    case Size.Value of
    1 : Props.FontSize := 8;
    2 : Props.FontSize := 10;
    3 : Props.FontSize := 12;
    4 : Props.FontSize := 14;
    5 : Props.FontSize := 18;
    6 : Props.FontSize := 24;
    7 : Props.FontSize := 36;
    end;
  hrsRelative :
    begin
      TmpSize := Props.BaseFontSize + Size.Value;
      if TmpSize <= 1 then                                             {!!.10}
        Props.FontSize := 8                                            {!!.10}
      else                                                             {!!.10}
        case TmpSize of
        {0,
        1 : Props.FontSize := 8;}                                      {!!.10}
        2 : Props.FontSize := 10;
        3 : Props.FontSize := 12;
        4 : Props.FontSize := 14;
        5 : Props.FontSize := 18;
        6 : Props.FontSize := 24;
        else
          Props.FontSize := 36;
        end;
    end;
  end;
  if Color <> -1 then
    Props.FontColor := Color;
end;

{!!.10 new}
constructor TIpHtmlNodeFONT.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  FSize := TIpHtmlRelSize.Create;
end;

destructor TIpHtmlNodeFONT.Destroy;
begin
  inherited;
  FSize.Free;
end;

procedure TIpHtmlNodeFONT.SetColor(const Value: TColorRef);
begin
  if Value <> FColor then begin
    FColor := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFONT.SetFace(const Value: string);
begin
  if Value <> FFace then begin
    FFace := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFONT.SizeChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeFontStyle }

procedure TIpHtmlNodeFontStyle.ApplyProps(
  const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  case Style of
  hfsTT :
    Props.FontName := Owner.FixedTypeface;                             {!!.10}
  hfsI :
    Props.FontStyle := Props.FontStyle + [fsItalic];
  hfsB :
    Props.FontStyle := Props.FontStyle + [fsBold];
  hfsU :
    Props.FontStyle := Props.FontStyle + [fsUnderline];
  hfsSTRIKE,
  hfsS :
    Props.FontStyle := Props.FontStyle + [fsStrikeout];
  hfsBIG :
    Props.FontSize := Props.FontSize + 2;
  hfsSMALL :
    Props.FontSize := Props.FontSize - 2;
  hfsSUB :
    begin
      Props.FontSize := Props.FontSize - 4;
      Props.FontBaseline := Props.FontBaseline - 2;
    end;
  hfsSUP :
    begin
      Props.FontSize := Props.FontSize - 4;
      Props.FontBaseline := Props.FontBaseline + 4;
    end;
  end;
end;

{ TIpHtmlNodeBlock }

constructor TIpHtmlNodeBlock.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create(ParentNode);
  ElementQueue := TList.Create;
  FMin := -1;
  FMax := -1;
  Props := TIpHtmlProps.Create(Owner);
end;

destructor TIpHtmlNodeBlock.Destroy;
begin
  ClearWordList;
  ElementQueue.Free;
  ElementQueue := nil;
  Props.Free;
  Props := nil;
  inherited;
end;

procedure TIpHtmlNodeBlock.RenderQueue;
var
  i : Integer;
  CurWord : PIpHtmlElement;
  LastProp : TIpHtmlProps;
  R : TRect;
  P : TPoint;
  L0 : Boolean;
  {$IFDEF IP_LAZARUS}
  OldBrushcolor: TColor;
  OldFontColor: TColor;
  OldBrushStyle: TBrushStyle;
  {$ENDIF}
begin
  L0 := Level0;
  LastProp := nil;


  for i := 0 to pred(ElementQueue.Count) do begin
    CurWord := PIpHtmlElement(ElementQueue[i]);

    if (CurWord.Props <> nil) and (CurWord.Props <> LastProp) then begin

      {$IFDEF IP_LAZARUS}
      Owner.Target.Font.BeginUpdate; // for speedup
      {$ENDIF}
      if (LastProp = nil) or not LastProp.AIsEqualTo(CurWord.Props) then
        with CurWord.Props do begin
          Owner.Target.Font.Name := FontName;
          if ScaleFonts then                                           {!!.10}
            Owner.Target.Font.Size := round(FontSize * Aspect)         {!!.10}
          else                                                         {!!.10}
            Owner.Target.Font.Size := FontSize;
          Owner.Target.Font.Style := FontStyle;
        end;
      if ScaleBitmaps and BWPRinter then                               {!!.10}
        Owner.Target.Font.Color := clBlack                             {!!.10}
      else                                                             {!!.10}
        if (LastProp = nil) or not LastProp.BIsEqualTo(CurWord.Props) then
          Owner.Target.Font.Color := CurWord.Props.FontColor;
      {$IFDEF IP_LAZARUS}
      Owner.Target.Font.EndUpdate;
      {$ENDIF}
      LastProp := CurWord.Props;
    end;

    if IntersectRect(R, CurWord.WordRect2, Owner.PageViewRect) then
      case CurWord.ElementType of
      etWord :
        begin
          P := Owner.PagePtToScreen(CurWord.WordRect2.TopLeft);
          {$IFDEF IP_LAZARUS}
          if CurWord.IsSelected  then begin
            OldBrushColor := Owner.Target.Brush.Color;
            OldBrushStyle := Owner.Target.Brush.Style;
            OldFontColor := Owner.Target.Font.Color;
            Owner.Target.Font.color := clHighlightText;
            Owner.Target.brush.Style := bsSolid;
            Owner.Target.brush.color := clHighLight;
            Owner.PageRectToScreen(curWord.WordRect2, R);
            Owner.Target.FillRect(R);
          end else
          {$ENDIF}
          Owner.Target.Brush.Style := bsClear;
          Owner.Target.TextOut(P.x, P.y, NoBreakToSpace(CurWord.AnsiWord));
          {$IFDEF IP_LAZARUS}
          if CurWord.IsSelected then begin
            Owner.Target.Font.Color := OldFontColor;
            Owner.Target.Brush.Color := OldBrushColor;
            Owner.Target.Brush.Style := OldBrushStyle;
          end;
          {$ENDIF}
          Owner.AddRect(CurWord.WordRect2, CurWord, Self);
        end;
      etObject :
        begin
          TIpHtmlNodeAlignInline(CurWord.Owner).Draw(Self);
          LastProp := nil;
        end;
      etSoftHyphen :
        begin
          P := Owner.PagePtToScreen(CurWord.WordRect2.TopLeft);
          Owner.Target.Brush.Style := bsClear;
          Owner.Target.TextOut(P.x, P.y, '-');
          Owner.AddRect(CurWord.WordRect2, CurWord, Self);
        end;
      end
    else
      case CurWord.ElementType of
      etWord,
      etObject,
      etSoftHyphen :
        if (CurWord.WordRect2.Bottom <> 0)
        and (CurWord.WordRect2.Top > Owner.PageViewRect.Bottom)
        and L0 then
          break;
      end;
  end;
end;

procedure TIpHtmlNodeBlock.Render(
  const RenderProps: TIpHtmlProps);
begin
  if not RenderProps.IsEqualTo(Props) then begin
    SetProps(RenderProps);
    Props.Assign(RenderProps);
  end;
  if ElementQueue.Count = 0 then
    Enqueue;
  RenderQueue;
end;

{!!.10 moved here from inside CalcMinMaxQueueWidth}
procedure TIpHtmlNodeBlock.UpdateCurrent(Start: Integer; CurProps : TIpHtmlProps);
{- update other words that use same properties as the
  one at Start with their lengths. Cuts down on the number
  of time the font properties need to be changed.}
var
  i : Integer;
  CurElement : PIpHtmlElement;                                       {!!.10}

    function GetExt(const S: string): TSize;
    begin
      Result := Owner.Target.TextExtent(
                NoBreakToSpace(S));
    end;

begin
  for i := ElementQueue.Count - 1 downto Start + 1 do begin
    CurElement := PIpHtmlElement(ElementQueue[i]);
    {case CurElement.ElementType of
    etWord :}                                                        {!!.10}
    if CurElement.ElementType = etWord then
      if CurElement.IsBlank = 0 then begin
      if (CurElement.Props = nil)
      or CurElement.Props.AIsEqualTo(CurProps) then begin
        {if CurElement.IsBlank = 0 then begin}
          if (CurElement.SizeProp <> CurProps.PropA) then begin
            CurElement.Size :=
              GetExt(CurElement.AnsiWord);
              {Owner.Target.TextExtent(
                NoBreakToSpace(CurElement.AnsiWord));}
            CurElement.SizeProp := CurProps.PropA;
          end;
        end;
      end;
    {end;}
  end;
end;

procedure TIpHtmlNodeBlock.CalcMinMaxQueueWidth(
  const RenderProps: TIpHtmlProps; var Min, Max: Integer);
var
  i,
  TextWidth : Integer;
  MinW, MaxW : Integer;
  CurElement : PIpHtmlElement;
  CurObj : TIpHtmlNodeAlignInline;
  LIndent, LIndentP : Integer;
  LastW,
  LastElement : Integer;
  NoBr : Boolean;
  IndentW : Integer;
  CurProps : TIpHtmlProps;
  CurFontName : string;
  CurFontSize : integer;
  CurFontStyle : TFontStyles;
  SizeOfSpace : TSize;
  SizeOfHyphen : TSize;

  procedure ApplyProps;
  var
    Changed : Boolean;
    TextMetrics : TTextMetric;
  begin
    with CurElement.Props do begin
      if (CurProps = nil) or not AIsEqualTo(CurProps) then begin
        Changed := False;

        if (CurProps = nil) or (CurFontName <> FontName) or (CurFontName = '') then begin
          Owner.Target.Font.Name := FontName;
          CurFontName := FontName;
          Changed := True;
        end;
        if (CurProps = nil) or (CurFontSize <> FontSize) or (CurFontSize = 0) then begin
          Owner.Target.Font.Size := FontSize;
          CurFontSize := FontSize;
          Changed := True;
        end;
        if (CurProps = nil) or (CurFontStyle <> FontStyle) then begin
          Owner.Target.Font.Style := FontStyle;
          CurFontStyle := FontStyle;
          Changed := True;
        end;
        if PropA.SizeOfSpaceKnown then begin
          SizeOfSpace := PropA.KnownSizeOfSpace;
          SizeOfHyphen := PropA.KnownSizeOfHyphen;
        end else begin
          SizeOfSpace := Owner.Target.TextExtent(' ');
          {$IFDEF IP_LAZARUS}
          if SizeOfSpace.CX=0 then begin
            DebugLn('TIpHtmlNodeBlock.CalcMinMaxQueueWidth Font not found "',Owner.Target.Font.Name,'" Size=',dbgs(Owner.Target.Font.Size));
          end;
          {$ENDIF}
          SizeOfHyphen := Owner.Target.TextExtent('-');
          PropA.SetKnownSizeOfSpace(SizeOfSpace);
          PropA.KnownSizeOfHyphen := SizeOfHyphen;
        end;
        if Changed then begin
          if PropA.tmHeight = 0 then begin
            GetTextMetrics(Owner.Target.Handle, TextMetrics);
            PropA.tmAscent := TextMetrics.tmAscent;
            PropA.tmDescent := TextMetrics.tmDescent;
            PropA.tmHeight := TextMetrics.tmHeight;
          end;
        end;
      end;
    end;
    CurProps := CurElement.Props;
  end;

  (* !!.10 moved up as global method
  procedure UpdateCurrent(Start: Integer);
  {- update other words that use same properties as the
    one at Start with their lengths. Cuts down on the number
    of time the font properties need to be changed.}
  var
    i : Integer;
    CurElement : PIpHtmlElement;                                       {!!.10}
  begin
    for i := Start + 1 to LastElement do begin
      CurElement := PIpHtmlElement(ElementQueue[i]);
      {case CurElement.ElementType of
      etWord :}                                                        {!!.10}
      if CurElement.ElementType = etWord then
        if CurElement.IsBlank = 0 then begin
        if (CurElement.Props = nil)
        or CurElement.Props.AIsEqualTo(CurProps) then begin
          {if CurElement.IsBlank = 0 then begin}
            if (CurElement.SizeProp <> CurProps.PropA) then begin
              CurElement.Size :=
                Owner.Target.TextExtent(
                  NoBreakToSpace(CurElement.AnsiWord));
              CurElement.SizeProp := CurProps.PropA;
            end;
          end;
        end;
      {end;}
    end;
  end;
  *)

begin
  Min := 0;
  Max := 0;
  if ElementQueue.Count = 0 then exit;
  LIndent := 0;
  LIndentP := 0;

  {trim trailing blanks}
  LastElement := ElementQueue.Count - 1;
  repeat
    if (LastElement >= 0) then begin
      CurElement := PIpHtmlElement(ElementQueue[LastElement]);
      case CurElement.ElementType of
      etWord :
        if CurElement.IsBlank <> 0 then
          dec(LastElement)
        else
          break
      else
        break;
      end;
    end else
      break;
  until false;

  CurProps := nil;
  CurFontName := '';
  CurFontSize := 0;
  CurFontStyle := [];
  Owner.Target.Font.Style := CurFontStyle;
  SizeOfSpace := Owner.Target.TextExtent(' ');
  SizeOfHyphen := Owner.Target.TextExtent('-');
  i := 0;
  NoBr := False;
  while i <= LastElement do begin
    TextWidth := 0;
    IndentW := 0;
    LastW := 0;
    while (i <= LastElement) do begin
      MinW := 0;
      CurElement := PIpHtmlElement(ElementQueue[i]);
      if CurElement.Props <> nil then
        ApplyProps;
      case CurElement.ElementType of
      etWord :
        begin
        {determine height and width of word}
        if CurElement.IsBlank <> 0 then begin
          if NoBr then begin
            MaxW := SizeOfSpace.cx * CurElement.IsBlank;
            MinW := MaxW + LastW;
          end else begin
            MinW := SizeOfSpace.cx * CurElement.IsBlank;
            MaxW := MinW;
          end;
        end else begin
          if (CurElement.SizeProp = CurProps.PropA) then
            MaxW := CurElement.Size.cx
          else begin
            CurElement.Size :=
              Owner.Target.TextExtent(
                NoBreakToSpace(CurElement.AnsiWord));
            MaxW := CurElement.Size.cx;
            CurElement.SizeProp := CurProps.PropA;
            UpdateCurrent(i, CurProps);
          end;
          MinW := MaxW + LastW;
        end;
        LastW := MinW;
        end;
      etObject :
        begin
          CurObj := TIpHtmlNodeAlignInline(CurElement.Owner);
          CurObj.CalcMinMaxWidth(MinW, MaxW);
          LastW := 0;
          CurProps := nil;
        end;
      etSoftLF..etClearBoth :
        begin
          if TextWidth + IndentW > Max then
            Max := TextWidth + IndentW;
          TextWidth := 0;
          MinW := 0;
          MaxW := 0;
          inc(i);
          break;
        end;
      etIndent :
        begin
          inc(LIndent);
          LIndentP := LIndent * StdIndent;
          if LIndentP > IndentW then
            IndentW := LIndentP;
          MinW := 0;
          MaxW := 0;
        end;
      etOutdent :
        begin
          if LIndent > 0 then begin
            dec(LIndent);
            LIndentP := LIndent * StdIndent;
          end;
        MinW := 0;
        MaxW := 0;
        end;
      etSoftHyphen :
        begin
          MaxW := SizeOfHyphen.cx;
          MinW := MaxW + LastW;
        end;
      end;
      inc(MinW, LIndentP);
      if MinW > Min then
        Min := MinW;
      inc(TextWidth, MaxW);
      inc(i);
    end;

    Max := MaxI2(Max, TextWidth + IndentW);
  end;
end;

procedure TIpHtmlNodeBlock.CalcMinMaxWidth(const RenderProps: TIpHtmlProps;
      var Min, Max: Integer);
begin
  if RenderProps.IsEqualTo(Props) and (FMin <> -1) and (FMax <> -1) then begin
    Min := FMin;
    Max := FMax;
    exit;
  end;
  SetProps(RenderProps);
  Props.Assign(RenderProps);
  if ElementQueue.Count = 0 then
    Enqueue;
  CalcMinMaxQueueWidth(Props, Min, Max);
  FMin := Min;
  FMax := Max;
end;

procedure TIpHtmlNodeBlock.ClearWordList;
begin
  if ElementQueue <> nil then
    ElementQueue.Clear;
end;

procedure TIpHtmlNodeBlock.EnqueueElement(
  const Entry: PIpHtmlElement);
begin
  ElementQueue.Add(Entry);
end;

procedure TIpHtmlNodeBlock.Invalidate;
var
  R : TRect;
begin
  if PageRectToScreen(PageRect, R) then
    Owner.InvalidateRect(R);
end;

function TIpHtmlNodeBlock.GetHeight(const RenderProps: TIpHtmlProps;
                                    const Width: Integer): Integer;
begin
  if LastW = Width then begin
    Result := LastH;
    exit;
  end;
  Layout(RenderProps,
      Rect(0, 0, Width, MaxInt));
  Result := PageRect.Bottom;
  LastH := Result;
  LastW := Width;
end;

procedure TIpHtmlNodeBlock.Layout(const RenderProps: TIpHtmlProps;
  const TargetRect: TRect);
begin
  if EqualRect(TargetRect, PageRect) then exit;
  if not RenderProps.IsEqualTo(Props) then begin
    SetProps(RenderProps);
    Props.Assign(RenderProps);
  end;
  if ElementQueue.Count = 0 then
    Enqueue;
  if SameDimensions(TargetRect, PageRect) then
    RelocateQueue(TargetRect.Left - PageRect.Left, TargetRect.Top - PageRect.Top)
  else
    LayoutQueue(Props, TargetRect);
end;

procedure TIpHtmlNodeBlock.RelocateQueue(const dx, dy: Integer);
var
  i : Integer;
  CurElement : PIpHtmlElement;
  R : TRect;
begin
  OffsetRect(FPageRect, dx, dy);
  for i := 0 to pred(ElementQueue.Count) do begin
    CurElement := PIpHtmlElement(ElementQueue[i]);
    R := CurElement.WordRect2;
    if R.Bottom <> 0 then begin
      OffsetRect(R, dx, dy);
      SetWordRect(CurElement, R);
    end;
  end;
end;

procedure TIpHtmlNodeBlock.LayoutQueue(
  const RenderProps: TIpHtmlProps; const TargetRect: TRect);
type
  TWordInfo = record
    BaseX     : Integer;
    BOff      : Integer;
    CurAsc    : Integer;
    Sz        : TSize;
    VA        : TIpHtmlVAlign3;
    Hs        : Integer;
  end;
  PWordInfo = ^TWordInfo;
const
  MAXWORDS = 65536;
type
  TWordList = array[0..pred(MAXWORDS)] of TWordInfo;
  PWordList = ^TWordList;
var
  Y,
  i, MaxHeight, j,
  MaxAscent, MaxDescent,
  TextWidth, Width : Integer;
  W : Integer;
  Size : TSize;
  MaxTextWidth : Integer;
  CurElement : PIpHtmlElement;
  Al, SaveAl : TIpHtmlAlign;
  VAL : TIpHtmlVAlign3;
  FirstWord, LastWord, {dx,} m, X0 : Integer;
  CurHeight, CurAscent, CurDescent : Integer;
  LineBreak : Boolean;
  LeftQueue : TList;
  RightQueue : TList;
  tmAscent,
  tmDescent,
  tmHeight : Integer;
  LIdent, RIdent : Integer;
  VRemainL,
  VRemainR : Integer;
  Clear : (cNone, cLeft, cRight, cBoth);
  BaseOffset : Integer;
  ExpLIndent,
  PendingIndent, PendingOutdent : Integer;
  ExpBreak : Boolean;
  LTrim : Boolean;
  RectWidth : Integer;
  FirstElement, LastElement : Integer;
  SizeOfSpace : TSize;
  SizeOfHyphen : TSize;
  PendingLineBreak : Boolean;
  Prefor : Boolean;
  TempCenter : Boolean;
  CurProps : TIpHtmlProps;
  SoftBreak : Boolean;
  IgnoreHardLF : Boolean;
  CanBreak : Boolean;
  LastBreakpoint : Integer;
  WordInfo : PWordList;
  {WordInfoCount : Integer;}                                           {!!.12}
  WordInfoSize : Integer;
  CurObj : TIpHtmlNodeAlignInline;
  HyphenSpace : Integer;
  SoftLF : Boolean;
  HyphensPresent : Boolean;

  procedure QueueLeadingObjects;
  var
    CurObj : TIpHtmlNodeAlignInline;
  begin
    while FirstElement <= LastElement do begin
      CurElement := PIpHtmlElement(ElementQueue[FirstElement]);
      case CurElement.ElementType of
      etObject :
        begin
          CurObj := TIpHtmlNodeAlignInline(CurElement.Owner);
          case CurObj.Align of
          hiaLeft :
            begin
              LeftQueue.Add(CurElement);
              inc(FirstElement);
            end;
          hiaRight :
            begin
              RightQueue.Add(CurElement);
              inc(FirstElement);
            end;
          else
            break;
          end;
        end else
        break;
      end;
    end;
  end;

  procedure DoLeftAligned;
  var
    CurObj : TIpHtmlNodeAlignInline;
  begin
    if (LeftQueue.Count > 0) and (VRemainL = 0) then begin
      while LeftQueue.Count > 0 do begin
        CurElement := LeftQueue[0];
        CurObj := TIpHtmlNodeAlignInline(CurElement.Owner);
        Size := CurObj.GetDim(RectWidth);
        Width := (TargetRect.Right - TargetRect.Left)
          - LIdent - RIdent - Size.cx - ExpLIndent;
        if Width < 0 then
          break;
        SetWordRect(CurElement,
          Rect(TargetRect.Left + LIdent,
            Y,
            TargetRect.Left + LIdent + Size.cx,
            Y + Size.cy));
        inc(LIdent, Size.cx);
        VRemainL := MaxI2(VRemainL, Size.cy);
        LeftQueue.Delete(0);
      end;
    end;
  end;

  procedure DoRightAligned;
  var
    CurObj : TIpHtmlNodeAlignInline;
  begin
    if (RightQueue.Count > 0) and (VRemainR = 0) then begin
      while RightQueue.Count > 0 do begin
        CurElement := RightQueue[0];
        CurObj := TIpHtmlNodeAlignInline(CurElement.Owner);
        Size := CurObj.GetDim(RectWidth);
        Width := (TargetRect.Right - TargetRect.Left)
          - LIdent - RIdent - Size.cx - ExpLIndent;
        if Width < 0 then
          break;
        SetWordRect(CurElement,
          Rect(TargetRect.Right - RIdent - Size.cx,
            Y,
            TargetRect.Right - RIdent,
            Y + Size.cy));
        inc(RIdent, Size.cx);
        VRemainR := MaxI2(VRemainR, Size.cy);
        RightQueue.Delete(0);
      end;
    end;
  end;

  procedure OutputLine;
  var
    WDelta, WMod, j : Integer;
    R : TRect;
    CurWordInfo : PWordInfo;
    dx: Integer;                                                       {!!.12}
  begin
    WDelta := 0;
    WMod := 0;
    case Al of
    haDefault,
    haLeft :
      dx := 0;
    haCenter :
      if Width >= TextWidth then
        dx := (Width - TextWidth) div 2
      else
        dx := 0;
    haRight :
      if Width >= TextWidth then
        dx := Width - TextWidth
      else
        dx := 0;
    haChar :
      if Width >= TextWidth then
        dx := (Width - TextWidth) div 2
      else
        dx := 0;
    else //haJustify :
      if i >= ElementQueue.Count then
        dx := 0
      else begin
        dx := 0;
        m := i - FirstWord - 2;
        if m > 0 then begin
          WDelta := (Width - TextWidth) div m;
          WMod := (Width - TextWidth) mod m;
        end;
      end;
    end;

    if Owner.PageHeight <> 0 then begin
      {if we're printing, adjust line's vertical offset to not
       straddle a page boundary}
      j := Y mod Owner.PageHeight;
      {only do this for 'small' objects, like text lines}
      if (MaxAscent + MaxDescent < 200)
      and (j + MaxAscent + MaxDescent > Owner.PageHeight) then
        inc(Y, ((j + MaxAscent + MaxDescent) - Owner.PageHeight));
    end;

    for j := FirstWord to LastWord do begin
      CurElement := PIpHtmlElement(ElementQueue[j]);
      CurWordInfo := @WordInfo[j - FirstWord];
      if CurWordInfo.Sz.cx <> 0 then begin
        R.Left := CurWordInfo.BaseX;
        R.Right := R.Left + CurWordInfo.Sz.cx;
        case CurWordInfo.VA of
        hva3Top :
          begin
            R.Top := Y;
            R.Bottom := Y + CurWordInfo.Sz.cy;
          end;
        hva3Middle :
          begin
            R.Top := Y + (MaxHeight - CurWordInfo.Sz.cy) div 2;
            R.Bottom := R.Top + CurWordInfo.Sz.cy;
          end;
        hva3Bottom :
          begin
            R.Top := Y + MaxHeight - CurWordInfo.Sz.cy;
            R.Bottom := R.Top + CurWordInfo.Sz.cy;
          end;
        hva3Default,
        hva3Baseline :
          begin
            if CurWordInfo.CurAsc >= 0 then
              R.Top := Y + MaxAscent - CurWordInfo.CurAsc
            else
              R.Top := Y;
            R.Bottom := R.Top + CurWordInfo.Sz.cy;
          end;
        end;
        if WMod <> 0 then begin
          OffsetRect(R, dx + WDelta + 1, 0);
          dec(WMod);
        end else
          OffsetRect(R, dx + WDelta, 0);
        SetWordRect(CurElement, R);
      end else
        SetWordRect(CurElement, NullRect);
    end;
  end;

  procedure DoClear;
  begin
    case Clear of
    cLeft :
      if VRemainL > 0 then begin
        inc(Y, VRemainL);
        VRemainL := 0;
        LIdent := 0;
      end;
    cRight :
      if VRemainR > 0 then begin
        inc(Y, VRemainR);
        VRemainR := 0;
        RIdent := 0;
      end;
    cBoth :
      begin
        inc(Y,
          MaxI2(VRemainL, VRemainR));
        VRemainL := 0;
        VRemainR := 0;
        LIdent := 0;
        RIdent := 0;
      end;
    end;
    Clear := cNone;
  end;

  procedure ApplyProps;
  var
    TextMetrics : TTextMetric;
  begin
    with CurElement.Props do begin
      if (CurProps = nil) or not AIsEqualTo(CurProps) then begin
        if PropA.SizeOfSpaceKnown then begin
          SizeOfSpace := PropA.KnownSizeOfSpace;
          SizeOfHyphen := PropA.KnownSizeOfHyphen;
        end else begin
          Owner.Target.Font.Name := FontName;
          Owner.Target.Font.Size := FontSize;
          Owner.Target.Font.Style := FontStyle;
          SizeOfSpace := Owner.Target.TextExtent(' ');
          SizeOfHyphen := Owner.Target.TextExtent('-');
          PropA.SetKnownSizeOfSpace(SizeOfSpace);
          PropA.KnownSizeOfHyphen := SizeOfHyphen;
        end;
        if PropA.tmHeight = 0 then begin
          Owner.Target.Font.Name := FontName;
          Owner.Target.Font.Size := FontSize;
          Owner.Target.Font.Style := FontStyle;
          GetTextMetrics(Owner.Target.Handle, TextMetrics);
          PropA.tmAscent := TextMetrics.tmAscent;
          PropA.tmDescent := TextMetrics.tmDescent;
          PropA.tmHeight := TextMetrics.tmHeight;
        end;
        tmHeight := PropA.tmHeight;
        tmAscent := PropA.tmAscent;
        tmDescent := PropA.tmDescent;
      end;
      if (CurProps = nil) or not BIsEqualTo(CurProps) then begin
        Al := Alignment;
        VAL := VAlignment;
        BaseOffset := FontBaseline;
        PreFor := Preformatted;
      end;
    end;
    CurProps := CurElement.Props;
  end;

  procedure InitMetrics;
  var
    TextMetrics : TTextMetric;
  begin
    GetTextMetrics(Owner.Target.Handle, TextMetrics);
    tmAscent := TextMetrics.tmAscent;
    tmDescent := TextMetrics.tmDescent;
    tmHeight := TextMetrics.tmHeight;
  end;

  {!!.10 rewritten
  procedure SetWordInfoLength(NewLength : Integer);
  begin
    if (WordInfo = nil) or (NewLength > WordInfoSize) then begin
      WordInfoSize := ((NewLength div 256) + 1) * 256;
      ReAllocMem(WordInfo, WordInfoSize * sizeof(TWordInfo));
    end;
  end;
  }
  {!!.10 rewritten}
  procedure SetWordInfoLength(NewLength : Integer);
  var
    NewWordInfoSize: Integer;
    {$IFNDEF IP_LAZARUS}
    NewWordInfo: PWordList;
    {$ENDIF}
  begin
    if (WordInfo = nil) or (NewLength > WordInfoSize) then begin
      NewWordInfoSize := ((NewLength div 256) + 1) * 256;
      {$IFDEF IP_LAZARUS code below does not check if WordInfo<>nil}
      ReallocMem(WordInfo,NewWordInfoSize * sizeof(TWordInfo));
      {$ELSE}
      NewWordInfo := AllocMem(NewWordInfoSize * sizeof(TWordInfo));
      move(WordInfo^, NewWordInfo^, WordInfoSize);
      Freemem(WordInfo);
      WordInfo := NewWordInfo;
      {$ENDIF}
      WordInfoSize := NewWordInfoSize;
    end;
  end;

  (*
  procedure DumpQueue;
  var
    i: Integer;
    CurElement : PIpHtmlElement;
  begin
    for i := 0 to ElementQueue.Count - 1 do begin
      CurElement := PIpHtmlElement(ElementQueue[i]);
      case CurElement.ElementType of
      etWord :
        write(' wrd:', CurElement.AnsiWord);
      etObject :
        write(' obj');
      etSoftLF :
        write(' softlf');
      etHardLF :
        write(' hardlf');
      etClearLeft :
        write(' clearleft');
      etClearRight :
        write(' clearright');
      etClearBoth :
        write(' clearboth');
      etIndent :
        write(' indent');
      etOutdent :
        write(' outdent');
      etSoftHyphen :
        write(' softhyphen');
      end;
    end;
    writeln('/////////////////');
  end;
  *)
  
begin
  if ElementQueue.Count = 0 then exit;
  {DumpQueue;} {debug}
  LeftQueue := nil;
  RightQueue := nil;
  WordInfoSize := 0;
  {WordInfoCount := 0;}                                                {!!.12}
  WordInfo := nil;
  try
    RectWidth := TargetRect.Right - TargetRect.Left;
    Y := TargetRect.Top;
    LeftQueue := TList.Create;
    RightQueue := TList.Create;
    SizeOfSpace := Owner.Target.TextExtent(' ');
    SizeOfHyphen := Owner.Target.TextExtent('-');
    InitMetrics;
    CurProps := nil;
    LIdent := 0;
    RIdent := 0;
    VRemainL := 0;
    VRemainR := 0;
    Clear := cNone;
    ExpLIndent := 0;
    PendingIndent := 0;
    PendingOutdent := 0;

    LastElement := ElementQueue.Count - 1;
    FirstElement := 0;
    QueueLeadingObjects;

    Prefor := False;
    ExpBreak := True;
    TempCenter := False;
    SaveAl := haLeft;
    IgnoreHardLF := False;

    LastBreakpoint := 0;

    FPageRect := TargetRect;
    i := 0;
    MaxHeight := 0;
    MaxAscent := 0;
    MaxDescent := 0;
    MaxTextWidth := 0;
    LineBreak := False;
    Al := haLeft;
    VAL := hva3Top;

    {trim trailing blanks}
    LastElement := ElementQueue.Count - 1;
    repeat
      if (LastElement >= FirstElement) then begin
        CurElement := PIpHtmlElement(ElementQueue[LastElement]);
        if (CurElement.ElementType = etWord) then
          if CurElement.IsBlank <> 0 then
            dec(LastElement)
          else
            break
        else
          break;
      end else
        break;
    until false;

    DoLeftAligned;
    DoRightAligned;

    i := FirstElement;
    CurAscent := 0;
    CurDescent := 0;
    CurHeight := 0;

    while i <= LastElement do begin

      if PendingIndent > PendingOutDent then begin
        if ExpLIndent < (TargetRect.Right - TargetRect.Left) - LIdent - RIdent then begin
          inc(ExpLIndent, (PendingIndent - PendingOutdent) * StdIndent);
        end;
      end else
      if PendingOutdent > PendingIndent then begin
        dec(ExpLIndent, (PendingOutDent - PendingIndent) * StdIndent);
        if ExpLIndent < 0 then
          ExpLIndent := 0;
      end;

      PendingIndent := 0;
      PendingOutdent := 0;

      DoLeftAligned;
      DoRightAligned;

      Width := (TargetRect.Right - TargetRect.Left)
        - LIdent - RIdent - ExpLIndent;

      LTrim := LineBreak or (ExpBreak and not PreFor) or (ExpLIndent > 0);

      W := Width; {total width we have}
      TextWidth := 0;
      FirstWord := i;
      LastWord := i-1;
      BaseOffset := 0;
      X0 := TargetRect.Left + LIdent + ExpLIndent;
      SoftBreak := False;
      HyphenSpace := 0;
      HyphensPresent := False;
      while (i < ElementQueue.Count) do begin
        CanBreak := False;
        CurElement := PIpHtmlElement(ElementQueue[i]);
        if CurElement.Props <> nil then
          ApplyProps;
        SoftLF := False;
        case CurElement.ElementType of
        etWord :
          begin
            IgnoreHardLF := False;
            if LTrim and (CurElement.IsBlank <> 0) then
              Size := SizeRec(0, 0)
            else begin
              if CurElement.IsBlank <> 0 then begin
                Size.cx := SizeOfSpace.cx * CurElement.IsBlank;
                Size.cy := SizeOfSpace.cy;
                CanBreak := True;
              end else begin
                if (CurElement.SizeProp = CurProps.PropA) then
                  Size := CurElement.Size
                else begin
                  Owner.Target.Font.Name := CurProps.FontName;
                  Owner.Target.Font.Size := CurProps.FontSize;
                  Owner.Target.Font.Style := CurProps.FontStyle;
                  CurElement.Size :=
                    Owner.Target.TextExtent(
                      NoBreakToSpace(CurElement.AnsiWord));
                  Size := CurElement.Size;
                  CurElement.SizeProp := CurProps.PropA;
                end;
              end;
              LTrim := False;
              LineBreak := False;
              ExpBreak := False;
            end;
            CurAscent := tmAscent;
            CurDescent := tmDescent;
            CurHeight := tmHeight;
          end;
        etObject :
          begin
            IgnoreHardLF := False;
            CurAscent := 0;
            CurDescent := 0;
            CanBreak := True;
            LineBreak := False;
            CurObj := TIpHtmlNodeAlignInline(CurElement.Owner);
            Size := CurObj.GetDim(Width);
            CurHeight := Size.cy;
            case Curobj.Align of
            hiaCenter :
              begin
                ExpBreak := False;
                LTrim := False;
                CurAscent := MaxAscent;
                CurDescent := Size.cy - MaxAscent;
                TempCenter := True;
                SaveAl := Al;
                Al := haCenter;
              end;
            hiaTop :
              begin
                ExpBreak := False;
                LTrim := False;
                CurAscent := -1;
                CurDescent := Size.cy;
              end;
            hiaMiddle
             :
              begin
                ExpBreak := False;
                LTrim := False;
                CurAscent := Size.cy div 2;
                CurDescent := Size.cy div 2;
              end;
            hiaBottom :
              begin
                ExpBreak := False;
                LTrim := False;
                CurAscent := Size.cy;
                CurDescent := 0;
              end;
            hiaLeft :
              begin
                LeftQueue.Add(CurElement);
                CurElement := nil;
                CurHeight := 0;
                Size.cx := 0;
                if LTrim then begin
                  inc(i);
                  break;
                end;
              end;
            hiaRight :
              begin
                RightQueue.Add(CurElement);
                CurElement := nil;
                CurHeight := 0;
                Size.cx := 0;
                if LTrim then begin
                  inc(i);
                  break;
                end;
              end;
            end;
          end;
        etSoftLF :
          begin
            PendingLineBreak := False;
            if LineBreak or ExpBreak then begin
              MaxAscent := 0;
              MaxDescent := 0;
            end else begin
              if MaxAscent = 0 then begin
                MaxAscent := MaxI2(MaxAscent, tmAscent);
                MaxDescent := MaxI2(MaxDescent, tmDescent);
              end;
              PendingLineBreak := True;
            end;
            ExpBreak := True;
            if LineBreak then
              MaxDescent := 0;
            inc(i);
            LastWord := i - 2;
            if PendingLineBreak then
              LineBreak := True;
            if not IgnoreHardLF then
              break;
            Size.cx := w + 1;
            SoftLF := True;
          end;
        etHardLF :
          begin
            ExpBreak := True;
            if MaxAscent = 0 then begin
              MaxAscent := MaxI2(MaxAscent, tmAscent);
              MaxDescent := MaxI2(MaxDescent, tmDescent);
            end;
            if LineBreak then
              MaxDescent := 0;
            LastWord := i - 1;
            if not IgnoreHardLF then begin
              inc(i);
              break;
            end;
            if LastWord < FirstWord then begin                         {!!.01}
              LastWord := FirstWord;                                   {!!.01}
              CanBreak := True;                                        {!!.01}
              inc(i);                                                  {!!.01}
            end;                                                       {!!.01}
          end;
        etClearLeft, etClearRight, etClearBoth :
          begin
            ExpBreak := True;
            case CurElement.ElementType of
            etClearLeft : Clear := cLeft;
            etClearRight : Clear := cRight;
            etClearBoth : Clear := cBoth;
            end;
            if LineBreak then
              MaxDescent := 0;
            inc(i);
            LastWord := i - 2;
            if not IgnoreHardLF then
              break;
          end;
        etIndent :
          begin
            CurAscent := 1;
            CurDescent := 0;
            CurHeight := 1;
            Size := SizeRec(0, 0);
            inc(PendingIndent);
            LTrim := True;
            IgnoreHardLF := True;
            CanBreak := True;
          end;
        etOutdent :
          begin
            IgnoreHardLF := False;
            CurAscent := 1;
            CurDescent := 0;
            CurHeight := 1;
            inc(PendingOutdent);
            CanBreak := True;
            Size := SizeRec(0, 0);                                     {!!.10}
          end;
        etSoftHyphen :
          begin
            IgnoreHardLF := False;
            Size := SizeOfHyphen;
            Size.cy := SizeOfSpace.cy;
            HyphenSpace := Size.cx;
            HyphensPresent := HyphenSpace > 0;
            CanBreak := True;
            LTrim := False;
            LineBreak := False;
            ExpBreak := False;
            CurAscent := tmAscent;
            CurDescent := tmDescent;
            CurHeight := tmHeight;
          end;
        end;
        if (Size.cx <= W) then begin {!!.10}
          if CanBreak then
            LastBreakPoint := i;
          MaxAscent := MaxI2(MaxAscent, CurAscent);
          MaxDescent := MaxI2(MaxDescent, CurDescent);
          MaxHeight := MaxI3(MaxHeight, CurHeight, MaxAscent + MaxDescent);
          {if word fits on line}
          {update width and height}
          if (CurElement <> nil) and (CurElement.ElementType = etIndent) then
            Size.cx := MinI2(W, StdIndent - ((X0 - TargetRect.Left) mod StdIndent));
          dec(W, Size.cx);
          inc(TextWidth, Size.cx);
          if CurElement <> nil then begin
            if HyphensPresent then
              for j := 0 to i - FirstWord - 1 do begin
                Assert(j < WordInfoSize);
                with WordInfo[j] do
                  if Hs > 0 then begin
                    inc(W, Hs);
                    dec(TextWidth, Hs);
                    dec(X0, Hs);
                    Hs := 0;
                    Sz.cx := 0;
                  end;
                end;
            SetWordInfoLength(i - FirstWord + 1);
            with WordInfo[i - FirstWord] do begin
              Sz := SizeRec(Size.cx, CurHeight);
              BaseX := X0;
              BOff := BaseOffset;
              CurAsc := CurAscent + BaseOffset;
              VA := VAL;
              Hs := HyphenSpace;
              HyphenSpace := 0;
            end;
          end;
          inc(X0, Size.cx);
          LastWord := i;
          inc(i);
        end else begin
          if HyphensPresent then
            if CurElement <> nil then begin
              for j := 0 to i - FirstWord - 2 do
                with WordInfo[j] do
                  if Hs > 0 then begin
                    dec(TextWidth, Hs);
                    Hs := 0;
                    Sz.cx := 0;
                  end;
            end;
          if CanBreak then
            LastBreakPoint := i - 1;
          if (LastWord >= 0) and (LastWord < ElementQueue.Count) then begin
            CurElement := PIpHtmlElement(ElementQueue[Lastword]);
            if (CurElement.ElementType = etWord)
            and (CurElement.IsBlank <> 0) then begin
              WordInfo[LastWord - FirstWord].Sz.cx := 0;
              LastWord := i - 2;
            end;
          end;
          LineBreak := True;
          SoftBreak := not SoftLF;
          break;
        end;
      end;

      if SoftBreak and (LastBreakPoint > 0) then
        LastWord := LastBreakPoint;

      OutputLine;

      {if SoftBreak and (LastBreakPoint > 0) then} {!!}
        {i := LastBreakPoint + 1;}                 {!!}

      if TempCenter then begin
        Al := SaveAl;
        TempCenter := False;
      end;

      if (TextWidth = 0) then begin
        if not ExpBreak and (VRemainL = 0) and (VRemainR = 0) then
          break;
      end;

      if TextWidth > MaxTextWidth then
        MaxTextWidth := TextWidth;
      inc(Y, MaxAscent + MaxDescent);
      if VRemainL > 0 then begin
        if SoftBreak and (TextWidth = 0) and (MaxAscent + MaxDescent = 0) then begin
          inc(Y, VRemainL);
          VRemainL := 0;
          LIdent := 0;
        end else begin
          dec(VRemainL, MaxAscent + MaxDescent);
          if VRemainL <= 0 then begin
            VRemainL := 0;
            LIdent := 0;
          end;
        end;
      end;
      if VRemainR > 0 then begin
        if SoftBreak and (TextWidth = 0) and (MaxAscent + MaxDescent = 0) then begin
          inc(Y, VRemainR);
          VRemainR := 0;
          RIdent := 0;
        end else begin
          dec(VRemainR, MaxAscent + MaxDescent);
          if VRemainR <= 0 then begin
            VRemainR := 0;
            RIdent := 0;
          end;
        end;
      end;
      MaxHeight := 0;
      MaxAscent := 0;
      MaxDescent := 0;
      {prepare for next line}
      DoClear;
    end;
    inc(Y,
      MaxI3(MaxAscent + MaxDescent, VRemainL, VRemainR));
    VRemainL := 0;
    VRemainR := 0;
    LIdent := 0;
    RIdent := 0;
    MaxDescent := 0;

    DoLeftAligned;
    DoRightAligned;

    inc(Y,
      MaxI3(MaxAscent + MaxDescent, VRemainL, VRemainR));

    FPageRect.Bottom := Y;
    {clean up}
  finally
    LeftQueue.Free;
    RightQueue.Free;
    if WordInfo <> nil then
      FreeMem(WordInfo);
  end;
end;

procedure TIpHtmlNodeBlock.InvalidateSize;
begin
  FMin := -1;
  FMax := -1;
  LastW := 0;
  LastH := 0;
  inherited;
end;

function TIpHtmlNodeBlock.Level0: Boolean;
var
  P : TIpHtmlNode;
begin
  Result := True;
  P := FParentNode;
  while P <> nil do begin
    if P is TIpHtmlNodeBlock then begin
      Result := False;
      break;
    end;
    P := P.FParentNode;
  end;
end;

procedure TIpHtmlNodeBlock.ReportCurDrawRects(Owner: TIpHtmlNode;
  M : TRectMethod);
var
  i : Integer;
  CurElement : PIpHtmlElement;
begin
  for i := 0 to pred(ElementQueue.Count) do begin
    CurElement := PIpHtmlElement(ElementQueue[i]);
    if CurElement.Owner = Owner then
      M(CurElement.WordRect2);
  end;
end;

procedure TIpHtmlNodeBlock.AppendSelection(var S: string);
var
  LastY, StartSelIndex, EndSelIndex, i : Integer;
  CurElement : PIpHtmlElement;
  R : TRect;
  LFDone : Boolean;
begin
  if not Owner.AllSelected then begin
    StartSelIndex := 0;
    while StartSelIndex < ElementQueue.Count do begin
      CurElement := PIpHtmlElement(ElementQueue[StartSelIndex]);
      R := CurElement.WordRect2;
      if R.Bottom = 0 then
      else
      if (R.Top > Owner.FStartSel.y) and (R.Bottom < Owner.FEndSel.y) then
        break
      else
      if PtInRect(R, Owner.FStartSel) or PtInRect(R, Owner.FEndSel) then
        break
      else
      if (R.Bottom < Owner.FStartSel.y) then
      else
      if (R.Top > Owner.FEndSel.Y) then
      else
        if (R.Left >= Owner.FStartSel.x) and (R.Right <= Owner.FEndSel.x) then
          break;
      inc(StartSelIndex);
    end;
    EndSelIndex := pred(ElementQueue.Count);
    while EndSelIndex >= 0 do begin
      CurElement := PIpHtmlElement(ElementQueue[EndSelIndex]);
      R := CurElement.WordRect2;
      if R.Bottom = 0 then
      else
      if (R.Top > Owner.FStartSel.y) and (R.Bottom < Owner.FEndSel.y) then
        break
      else
      if PtInRect(R, Owner.FStartSel) or PtInRect(R, Owner.FEndSel) then
        break
      else
      if (R.Bottom < Owner.FStartSel.y) then
      else
      if (R.Top > Owner.FEndSel.Y) then
      else
        if (R.Left >= Owner.FStartSel.x) and (R.Right <= Owner.FEndSel.x) then
          break;
      dec(EndSelIndex);
    end;
  end else begin
    StartSelIndex := 0;
    EndSelIndex := ElementQueue.Count - 1;
  end;
  LastY := -1;
  LFDone := True;
  for i := StartSelIndex to EndSelIndex do begin
    CurElement := PIpHtmlElement(ElementQueue[i]);
    R := CurElement.WordRect2;
    if not LFDone and (R.Top <> LastY) then begin
      S := S + #13#10;
      LFDone := True;
    end;
    case CurElement.ElementType of
    etWord :
      begin
        S := S + NoBreakToSpace(CurElement.AnsiWord);
        LFDone := False;
      end;
    etObject :
      begin
        TIpHtmlNodeAlignInline(CurElement.Owner).AppendSelection(S);
        LFDone := False;
      end;
    etSoftLF..etClearBoth :
      if not LFDone then begin
        S := S + #13#10;
        LFDone := True;
      end;
    end;
    LastY := R.Top;
  end;
end;

{!!.10 new}
function TIpHtmlNodeBlock.ElementQueueIsEmpty: Boolean;
begin
  Result := ElementQueue.Count = 0;
end;

{ TIpHtmlNodeP }

constructor TIpHtmlNodeP.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  Props := TIpHtmlProps.Create(Owner);
end;

destructor TIpHtmlNodeP.Destroy;
begin
  Props.Free;
  inherited;
end;

procedure TIpHtmlNodeP.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.Alignment := Align;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeP.Enqueue;
begin
  if FChildren.Count > 0 then begin
    if not (FParentNode is TIpHtmlNodeLI) then begin                   {!!.10}
      EnqueueElement(Owner.SoftLF);
      EnqueueElement(Owner.HardLF);
    end;
  end;
  inherited Enqueue;
  if FChildren.Count > 0 then begin
    EnqueueElement(Owner.SoftLF);
    EnqueueElement(Owner.HardLF);
  end;
end;

procedure TIpHtmlNodeP.SetAlign(const Value: TIpHtmlAlign);
begin
  if Value <> FAlign then begin
    FAlign := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeOBJECT }

{!!.10}
destructor TIpHtmlNodeOBJECT.Destroy;
begin
  inherited;
  FWidth.Free;
end;

procedure TIpHtmlNodeOBJECT.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeOL }

procedure TIpHtmlNodeOL.Enqueue;
var
  i : Integer;
begin
  {render list}
  if FChildren.Count > 0 then begin
    EnqueueElement(Owner.SoftLF);
  end;
  FParentNode.EnqueueElement(Owner.LIndent);
  for i := 0 to pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeLI then begin
      Counter := i + 1;
      TIpHtmlNodeLI(FChildren[i]).Enqueue;
      FParentNode.EnqueueElement(Owner.SoftLF);
    end else
      TIpHtmlNode(FChildren[i]).Enqueue;
  FParentNode.EnqueueElement(Owner.LOutdent);
  FParentNode.EnqueueElement(Owner.SoftLF);
end;

function TIpHtmlNodeOL.GetNumString: string;

  function IntToRomanStr(i : Integer): string;
  const
    RC : array[0..6] of AnsiChar = ('M', 'D', 'C', 'L', 'X', 'V', 'I');
    RV : array[0..6] of Integer = (1000, 500, 100, 50, 10, 5, 1);
  var
    n : Integer;
  begin
    Result := '';
    n := 0;
    repeat
      while i >= RV[n] do begin
        Result := Result + RC[n];
        dec(i, RV[n]);
      end;
      inc(n);
    until i = 0;
  end;

begin
  {$IFDEF IP_LAZARUS}
  Result := ''; // stop warning
  {$ENDIF}
  case Style of
  olArabic :
    str(Counter, Result);
  olLowerAlpha :
    Result := chr(ord('a') + Counter - 1);
  olUpperAlpha :
    Result := chr(ord('A') + Counter - 1);
  olLowerRoman :
    Result := LowerCase(IntToRomanStr(Counter));
  olUpperRoman :
    Result := IntToRomanStr(Counter);
  end;
end;

procedure TIpHtmlNodeOL.SetStart(const Value: Integer);
begin
  if Value <> FStart then begin
    FStart := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeOL.SetStyle(const Value: TIpHtmlOLStyle);
begin
  if Value <> FStyle then begin
    FStyle := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeList }

procedure TIpHtmlNodeList.Enqueue;
var
  i : Integer;
begin
  if FChildren.Count > 0 then begin
    EnqueueElement(Owner.SoftLF);
  end;
  {render list}
  FParentNode.EnqueueElement(Owner.LIndent);
  for i := 0 to pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeLI then begin
      TIpHtmlNodeLI(FChildren[i]).Enqueue;
      FParentNode.EnqueueElement(Owner.SoftLF);
    end else
      TIpHtmlNode(FChildren[i]).Enqueue;
  FParentNode.EnqueueElement(Owner.LOutdent);
  EnqueueElement(Owner.SoftLF);
end;

procedure TIpHtmlNodeList.SetListType(const Value: TIpHtmlULType);
begin
  if Value <> FListType then begin
    FListType := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeHeader }

constructor TIpHtmlNodeHeader.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  Props := TIpHtmlProps.Create(Owner);
end;

destructor TIpHtmlNodeHeader.Destroy;
begin
  Props.Free;
  inherited;
end;

procedure TIpHtmlNodeHeader.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  case Size of
  1 : Props.FontSize := 24;
  2 : Props.FontSize := 18;
  3 : Props.FontSize := 14;
  4 : Props.FontSize := 12;
  5 : Props.FontSize := 10;
  6 : Props.FontSize :=  8;
  end;
  Props.FontStyle := [fsBold];
  Props.Alignment := Align;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeHeader.Enqueue;
begin
  if FChildren.Count > 0 then
    EnqueueElement(Owner.HardLF);
  inherited Enqueue;
  if FChildren.Count > 0 then begin
    EnqueueElement(Owner.SoftLF);
    EnqueueElement(Owner.HardLF);
  end;
end;

{ TIpHtmlNodeLI }

procedure TIpHtmlNodeLI.CalcMinMaxWidth(var Min, Max: Integer);
begin
  if ScaleBitmaps then begin                                           {!!.10}
    Min := round(8 * Aspect);                                          {!!.10}
    Max := round(8 * Aspect);                                          {!!.10}
  end else begin                                                       {!!.10}
    Min := 8;
    Max := 8;
  end;
end;

constructor TIpHtmlNodeLI.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  Align := hiaBottom;
  WordEntry := Owner.NewElement(etWord, Self);
  WordEntry.Props := Props;
end;

procedure TIpHtmlNodeLI.Draw;
var
  R : TRect;
  SaveColor : Tcolor;
begin
  if PageRectToScreen(GrossDrawRect, R) then
    case ListType of
    ulDisc :
      begin
        SaveColor := Owner.Target.Brush.Color;
        Owner.Target.Brush.Color := Props.FontColor;
        if ScaleBitmaps then                                           {!!.10}
          Owner.Target.Ellipse(R.Left, R.Top, R.Left + round(7 * Aspect), R.Top + round(7 * Aspect))
        else
          Owner.Target.Ellipse(R.Left, R.Top, R.Left + 7, R.Top + 7);
        Owner.Target.Brush.Color := SaveColor;
      end;
    ulSquare :
      begin
        if ScaleBitmaps then                                           {!!.10}
          Owner.Target.Rectangle(R.Left, R.Top, R.Left + round(7 * Aspect), R.Top + round(7 * Aspect))
        else
          Owner.Target.Rectangle(R.Left, R.Top, R.Left + 7, R.Top + 7);
      end;
    ulCircle :
      begin
        if ScaleBitmaps then                                           {!!.10}
          Owner.Target.Ellipse(R.Left, R.Top, R.Left + round(7 * Aspect), R.Top + round(7 * Aspect))
        else
          Owner.Target.Ellipse(R.Left, R.Top, R.Left + 7, R.Top + 7);
      end;
    end;
end;

procedure TIpHtmlNodeLI.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeLI.Enqueue;
var
  S : string;
  i : Integer;
begin
  if FParentNode is TIpHtmlNodeOL then begin
    S := TIpHtmlNodeOL(FParentNode).GetNumString;
    SetRawWordValue(WordEntry, S + '.');
    EnqueueElement(WordEntry);
  end else
    EnqueueElement(Element);
  EnqueueElement(Owner.LIndent);
  for i := 0 to pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).Enqueue;
  EnqueueElement(Owner.LOutdent);
end;

function TIpHtmlNodeLI.GetDim(ParentWidth: Integer): TSize;
begin
  if ScaleBitmaps then                                                 {!!.10}
    Result := SizeRec(round(Aspect * 8), round(Aspect * 8))            {!!.10}
  else                                                                 {!!.10}
    Result := SizeRec(8, 8);
end;

function TIpHtmlNodeLI.GrossDrawRect: TRect;
begin
  Result := PIpHtmlElement(Element).WordRect2;
end;

procedure TIpHtmlNodeLI.SetListType(const Value: TIpHtmlULType);
begin
  if Value <> FListType then begin
    FListType := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeLI.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeBR }

procedure TIpHtmlNodeBR.Enqueue;
begin
  case Clear of
  hbcNone :
    EnqueueElement(Owner.HardLF);
  hbcLeft :
    EnqueueElement(Owner.HardLFClearLeft);
  hbcRight :
    EnqueueElement(Owner.HardLFClearRight);
  hbcAll :
    EnqueueElement(Owner.HardLFClearBoth);
  end;
end;

procedure TIpHtmlNodeBR.SetClear(const Value: TIpHtmlBreakClear);
begin
  FClear := Value;
  InvalidateSize;
end;

{ TIpHtmlNodeHR }

constructor TIpHtmlNodeHR.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FColor := -1;
  Align := hiaCenter;
  SizeWidth := TIpHtmlPixels.Create;
end;

procedure TIpHtmlNodeHR.Draw;
var
  R : TRect;
  TopLeft : TPoint;
  Dim : TSize;
  SaveBrushColor,
  SavePenColor : TColor;
begin
  TopLeft := GrossDrawRect.TopLeft;
  R.TopLeft := TopLeft;
  Dim := GetDim(0);
  R.Right := TopLeft.x + Dim.cx;
  R.Bottom := TopLeft.y + Dim.cy;
  if not PageRectToScreen(R, R) then
    exit;
  if NoShade or (Color <> -1) then begin
    SavePenColor := Owner.Target.Pen.Color;
    SaveBrushColor := Owner.Target.Brush.Color;
    if Color = -1 then begin
      Owner.Target.Pen.Color := clBlack;
      Owner.Target.Brush.Color := clBlack;
    end else begin
      Owner.Target.Pen.Color := Color;
      Owner.Target.Brush.Color := Color;
    end;
    Owner.Target.FillRect(R);
    Owner.Target.Pen.Color := SavePenColor;
    Owner.Target.Brush.Color := SaveBrushColor;
  end else begin
    SavePenColor := Owner.Target.Pen.Color;
    SaveBrushColor := Owner.Target.Brush.Color;
    Owner.Target.Pen.Color := clGray;
    Owner.Target.Brush.Color := clGray;
    Owner.Target.FillRect(R);
    Owner.Target.Pen.Color := clWhite;
    Owner.Target.MoveTo(R.Left - 1, R.Bottom + 1);
    Owner.Target.LineTo(R.Left - 1, R.Top - 1);
    Owner.Target.LineTo(R.Right + 1, R.Top - 1);
    Owner.Target.Pen.Color := clBlack;
    Owner.Target.LineTo(R.Right + 1, R.Bottom + 1);
    Owner.Target.LineTo(R.Left - 1, R.Bottom + 1);
    Owner.Target.Pen.Color := SavePenColor;
    Owner.Target.Brush.Color := SaveBrushColor;
  end;
end;

function TIpHtmlNodeHR.GetDim(ParentWidth: Integer): TSize;
begin
  if (SizeWidth.PixelsType <> hpAbsolute)
  or ((ParentWidth <> 0) and (SizeWidth.Value <> ParentWidth)) then begin
    case Width.LengthType of
    hlUndefined :
      FDim.cx := 0;
    hlAbsolute :
      FDim.cx := Width.LengthValue;
    hlPercent :
      FDim.cx := round(ParentWidth * Width.LengthValue / 100);
    end;
    FDim.cy := Size.Value;
    SizeWidth.Value := ParentWidth;
    SizeWidth.PixelsType := hpAbsolute;
  end;
  Result := FDim;
end;

function TIpHtmlNodeHR.GrossDrawRect: TRect;
begin
  Result := PIpHtmlElement(Element).WordRect2;
end;

procedure TIpHtmlNodeHR.CalcMinMaxWidth(var Min, Max: Integer);
begin
  Min := 0;
  Max := 0;
  case Width.LengthType of
  hlAbsolute :
    begin
      Min := Width.LengthValue;
      Max := Min;
    end;
  end;
end;

procedure TIpHtmlNodeHR.Enqueue;
begin
  EnqueueElement(Owner.SoftLF);
  inherited;
  EnqueueElement(Owner.SoftLF);
end;

{!!.10 new}
destructor TIpHtmlNodeHR.Destroy;
begin
  inherited;
  FWidth.Free;
  SizeWidth.Free;
  FSize.Free;                                                          {!!.10}
end;

procedure TIpHtmlNodeHR.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeA }

procedure TIpHtmlNodeA.AddArea(const R: TRect);
var
  RCopy : PRect;
  c : Integer;
begin
  c := AreaList.Count;
  if c > 0 then begin
    RCopy := PRect(AreaList[c-1]);
    if (R.Left = RCopy.Right)
    and (R.Top = RCopy.Top)
    and (R.Bottom = RCopy.Bottom) then begin
      RCopy.Right := R.Right;
      exit;
    end;
  end;
  New(RCopy);
  RCopy^  := R;
  AreaList.Add(RCopy);
end;

procedure TIpHtmlNodeA.AddMapArea(const R: TRect);
var
  RCopy : PRect;
  c : Integer;
begin
  c := MapAreaList.Count;
  if c > 0 then begin
    RCopy := PRect(AreaList[c-1]);
    if (R.Left = RCopy.Right)
    and (R.Top = RCopy.Top)
    and (R.Bottom = RCopy.Bottom) then begin
      RCopy.Right := R.Right;
      exit;
    end;
  end;
  New(RCopy);
  RCopy^  := R;
  MapAreaList.Add(RCopy);
end;

procedure TIpHtmlNodeA.ClearAreaList;
var
  a: Pointer;
  m: Pointer;
begin
  while AreaList.Count > 0 do begin
    a:=AreaList[0];
    FreeMem(a);
    AreaList.Delete(0);
  end;
  while MapAreaList.Count > 0 do begin
    m:=MapAreaList[0];
    FreeMem(m);
    MapAreaList.Delete(0);
  end;
end;

constructor TIpHtmlNodeA.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  AreaList := TList.Create;
  MapAreaList := TList.Create;
  Props := TIpHtmlProps.Create(Owner);
end;

destructor TIpHtmlNodeA.Destroy;
begin
  if HasRef then
    Owner.AnchorList.Remove(Self);
  Props.Free;
  ClearAreaList;
  AreaList.Free;
  MapAreaList.Free;
  inherited;
end;

procedure TIpHtmlNodeA.BuildAreaList;
var
  i : Integer;
begin
  for i := 0 to pred(FChildren.Count) do begin
    TIpHtmlNode(FChildren[i]).ReportDrawRects(AddArea);
    TIpHtmlNode(FChildren[i]).ReportMapRects(AddMapArea);
  end;
end;

function TIpHtmlNodeA.PtInRects(const P: TPoint): Boolean;
var
  i : Integer;
begin
  if AreaList.Count = 0 then
    BuildAreaList;
  for i := 0 to pred(AreaList.Count) do begin
    with PRect(AreaList[i])^ do
    if PtInRect(PRect(AreaList[i])^,P) then begin
      Result := True;
      exit;
    end;
  end;
  Result := False;
end;

function TIpHtmlNodeA.RelMapPoint(const P: TPoint): TPoint;
var
  i : Integer;
begin
  if AreaList.Count = 0 then
    BuildAreaList;
  for i := 0 to pred(MapAreaList.Count) do begin
    with PRect(MapAreaList[i])^ do
    if PtInRect(PRect(AreaList[i])^,P) then begin
      Result := Point(
        P.x - PRect(AreaList[i])^.Left,
        P.y - PRect(AreaList[i])^.Top);
      exit;
    end;
  end;
  Result := Point(-1, -1);
end;

procedure TIpHtmlNodeA.SetHot(const Value: Boolean);
var
  i : Integer;
  R : TRect;
begin
  FHot := Value;
  if AreaList.Count = 0 then
    BuildAreaList;
  SetProps(Props);
  for i := 0 to pred(AreaList.Count) do
    if PageRectToScreen(PRect(AreaList[i])^, R) then
      Owner.InvalidateRect(R);
end;

procedure TIpHtmlNodeA.SetHRef(const Value: string);
var
  NewHasRef : Boolean;
begin
  FHRef := Value;
  NewHasRef := Value <> '';
  if NewHasRef <> HasRef then begin
    if HasRef then
      Owner.AnchorList.Remove(Self)
    else
      Owner.AnchorList.Add(Self);
    FHasRef := NewHasRef;
  end;
end;

procedure TIpHtmlNodeA.DoOnBlur;
begin
  {FHasFocus := False;}                                                {!!.12}
  Hot := False;
end;

procedure TIpHtmlNodeA.DoOnFocus;
begin
  {FHasFocus := True;}                                                 {!!.12}
  MakeVisible;
  Hot := True;
end;

procedure TIpHtmlNodeA.SetName(const Value: string);
begin
  if FName <> '' then
    with Owner.NameList do
      Delete(IndexOf(FName));
  FName := Value;
  if FName <> '' then
    Owner.NameList.AddObject(FName, Self);
end;

procedure TIpHtmlNodeA.MakeVisible;
var
  i : Integer;
  R : TRect;
begin
  if AreaList.Count = 0 then
    BuildAreaList;
  SetRectEmpty(R);
  for i := 0 to pred(AreaList.Count) do
    UnionRect(R, R, PRect(AreaList[i])^);
  Owner.MakeVisible(R);
end;

procedure TIpHtmlNodeA.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);        
  if FHot then begin
    Props.FontColor := Props.ALinkColor;
    Props.FontStyle := Props.FontStyle + [fsUnderline];
  end else
    if HasRef then begin
      Props.FontStyle := Props.FontStyle + [fsUnderline];
      if Owner.LinkVisited(HRef) then
        Props.FontColor := Props.VLinkColor
      else
        Props.FontColor := Props.LinkColor;
    end;
  inherited SetProps(Props);
end;

function TIpHtmlNodeA.GetHint: string;
begin
  Result := HRef;
end;

{ TIpHtmlNodeDIV }

constructor TIpHtmlNodeDIV.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  Props := TIpHtmlProps.Create(Owner);
end;

destructor TIpHtmlNodeDIV.Destroy;
begin
  Props.Free;
  inherited;
end;

procedure TIpHtmlNodeDIV.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.Alignment := Align;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeDIV.Enqueue;
begin
  if FChildren.Count > 0 then
    EnqueueElement(Owner.HardLF);
  inherited Enqueue;
  if FChildren.Count > 0 then
    EnqueueElement(Owner.HardLF);
end;

{ TIpHtmlNodeSPAN }

procedure TIpHtmlNodeSPAN.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.Alignment := Align;
end;

{ TIpHtmlNodeTABLE }

procedure TIpHtmlNodeTABLE.CalcMinMaxColTableWidth(
                         const RenderProps: TIpHtmlProps;var Min, Max: Integer);
var
  z, Min0, Max0: Integer;
  i, j, CurCol, k : Integer;
  TWMin, TWMax : Integer;
  PendSpanWidthMin,
  PendSpanWidthMax,
  PendSpanStart,
  PendSpanSpan : TIntArr;
  PendCol : Integer;

  procedure DistributeColSpace(ColSpan: Integer);
  var
    i, Rest, MinNow : Integer;
  begin
    if ColSpan > 1 then begin
      PendSpanWidthMin[PendCol] := Min0;
      PendSpanWidthMax[PendCol] := Max0;
      PendSpanStart[PendCol] := CurCol;
      PendSpanSpan[PendCol] := ColSpan;
      inc(PendCol);
      exit;
    end;
    MinNow := 0;
    for i := CurCol to CurCol + ColSpan - 1 do
      inc(MinNow, ColTextWidthMin[i]);
    if MinNow = 0 then begin
      for i := CurCol to CurCol + ColSpan - 1 do
         ColTextWidthMin[i] := Min0 div ColSpan;
    end else begin
      Rest := Min0 - MinNow;
      if Rest > 0 then begin
        for i := CurCol to CurCol + ColSpan - 1 do
           {inc(ColTextWidthMin[i],
             round(Rest * ColTextWidthMin[i] / MinNow));}              {!!.10}
           ColTextWidthMin[i] := ColTextWidthMin[i] +                  {!!.10}
             round(Rest * ColTextWidthMin[i] / MinNow);                {!!.10}
        MinNow := 0;
        for i := CurCol to CurCol + ColSpan - 1 do
          inc(MinNow, ColTextWidthMin[i]);
        Rest := Min0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do begin
            {inc(ColTextWidthMin[i]);}                                 {!!.10}
            ColTextWidthMin[i] := ColTextWidthMin[i] + 1;              {!!.10}
            dec(Rest);
            if rest = 0 then
              break;
          end;
        end;
      end;
    end;
    MinNow := 0;
    for i := CurCol to CurCol + ColSpan - 1 do
      inc(MinNow, ColTextWidthMax[i]);
    if MinNow = 0 then begin
      for i := CurCol to CurCol + ColSpan - 1 do
         ColTextWidthMax[i] := Max0 div ColSpan;
    end else begin
      Rest := Max0 - MinNow;
      if Rest > 0 then begin
        for i := CurCol to CurCol + ColSpan - 1 do
           {inc(ColTextWidthMax[i],
             round(Rest * ColTextWidthMax[i] / MinNow))}               {!!.10}
          ColTextWidthMax[i] := ColTextWidthMax[i] +                   {!!.10}
            round(Rest * ColTextWidthMax[i] / MinNow);                 {!!.10}
        MinNow := 0;
        for i := CurCol to CurCol + ColSpan - 1 do
          inc(MinNow, ColTextWidthMax[i]);
        Rest := Max0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do begin
            {inc(ColTextWidthMax[i]);}                                 {!!.10}
            ColTextWidthMax[i] := ColTextWidthMax[i] + 1;              {!!.10}
            dec(Rest);
            if rest = 0 then
              break;
          end;
        end;
      end;
    end;
    for i := 0 to pred(ColCount) do begin
      ColTextWidthMin[i] := MinI2(ColTextWidthMin[i], ColTextWidthMax[i]);
      ColTextWidthMax[i] := MaxI2(ColTextWidthMin[i], ColTextWidthMax[i]);
    end;
  end;

  procedure DistributeSpannedColSpace;
  var
    z, i, Rest, MinNow, Min0, Max0, CurCol, ColSpan : Integer;
  begin
    for z := 0 to pred(PendCol) do begin
      Min0 := PendSpanWidthMin[z];
      Max0 := PendSpanWidthMax[z];
      CurCol := PendSpanStart[z];
      ColSpan := PendSpanSpan[z];
      MinNow := 0;
      for i := CurCol to CurCol + ColSpan - 1 do
        inc(MinNow, ColTextWidthMin[i]);
      if MinNow = 0 then begin
        Rest := 0;                                                     {!!.10}
        for i := CurCol to CurCol + ColSpan - 1 do begin               {!!.10}
           ColTextWidthMin[i] := Min0 div ColSpan;
           inc(Rest, ColTextWidthMin[i]);                              {!!.10}
        end;
        ColTextWidthMin[0] := ColTextWidthMin[0] + (Min0 - Rest);      {!!.10}
      end else begin
        Rest := Min0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do
             {inc(ColTextWidthMin[i],
               round(Rest * ColTextWidthMin[i] / MinNow));}            {!!.10}
            ColTextWidthMin[i] := ColTextWidthMin[i] +                 {!!.10}
              round(Rest * ColTextWidthMin[i] / MinNow);               {!!.10}
          MinNow := 0;
          for i := CurCol to CurCol + ColSpan - 1 do
            inc(MinNow, ColTextWidthMin[i]);
          Rest := Min0 - MinNow;
          if Rest > 0 then begin
            for i := CurCol to CurCol + ColSpan - 1 do begin
              {inc(ColTextWidthMin[i]);}                               {!!.10}
              ColTextWidthMin[i] := ColTextWidthMin[i] + 1;            {!!.10}
              dec(Rest);
              if rest = 0 then
                break;
            end;
          end;
        end;
      end;
      MinNow := 0;
      for i := CurCol to CurCol + ColSpan - 1 do
        inc(MinNow, ColTextWidthMax[i]);
      if MinNow = 0 then begin
        Rest := 0;                                                     {!!.10}
        for i := CurCol to CurCol + ColSpan - 1 do begin
           ColTextWidthMax[i] := Max0 div ColSpan;
           inc(Rest, ColTextWidthMax[i]);                              {!!.10}
        end;
        ColTextWidthMax[0] := ColTextWidthMax[0] + (Max0 - Rest);      {!!.10}
      end else begin
        Rest := Max0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do
             {inc(ColTextWidthMax[i],
               round(Rest * ColTextWidthMax[i] / MinNow));}            {!!.10}
            ColTextWidthMax[i] := ColTextWidthMax[i] +                 {!!.10}
              round(Rest * ColTextWidthMax[i] / MinNow);               {!!.10}
          MinNow := 0;
          for i := CurCol to CurCol + ColSpan - 1 do
            inc(MinNow, ColTextWidthMax[i]);
          Rest := Max0 - MinNow;
          if Rest > 0 then begin
            for i := CurCol to CurCol + ColSpan - 1 do begin
              {inc(ColTextWidthMax[i]);}                               {!!.10}
              ColTextWidthMax[i] := ColTextWidthMax[i] + 1;            {!!.10}
              dec(Rest);
              if rest = 0 then
                break;
            end;
          end;
        end;
      end;
      for i := 0 to pred(ColCount) do begin
        {ColTextWidthMin[i] := MinI2(ColTextWidthMin[i], ColTextWidthMax[i]);} {!!.10}
        ColTextWidthMax[i] := MaxI2(ColTextWidthMin[i], ColTextWidthMax[i]);
      end;
    end;
  end;

  (*
  procedure BumpPercentages;
  var
    i, j, k, z : Integer;
    MaxPercent, Pix : Integer;
  begin
    for i := 0 to pred(ColCount) do
      RowSp[i] := 0;
    for z := 0 to pred(FChildren.Count) do
      if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
        with TIpHtmlNodeCore(FChildren[z]) do
          for i := 0 to pred(FChildren.Count) do begin
            if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
              with TIpHtmlNodeTR(FChildren[i]) do begin
                MaxPercent := 0;
                CurCol := 0;
                while RowSp[CurCol] <> 0 do begin
                  dec(RowSp[CurCol]);
                  inc(CurCol);
                end;
                for j := 0 to pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                      case Width.LengthType of
                      hlPercent :
                        begin
                          Pix := 0;
                          for k := 0 to pred(ColSpan) do
                            if RowSp[CurCol + k] = 0 then
                              inc(Pix, ColTextWidthMin[CurCol + k]);
                          Pix := round(100 * Pix / Width.LengthValue);
                          if Pix > MaxPercent then
                            MaxPercent := Pix;
                        end;
                      end;
                      for k := 0 to pred(ColSpan) do begin
                        while RowSp[CurCol] <> 0 do
                          inc(CurCol);
                        inc(CurCol);
                      end;
                    end;
                CurCol := 0;
                while RowSp[CurCol] <> 0 do begin
                  dec(RowSp[CurCol]);
                  inc(CurCol);
                end;
                for j := 0 to pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                      case Width.LengthType of
                      hlPercent :
                        if MaxPercent > 0 then begin
                          Pix := 0;
                          for k := 0 to pred(ColSpan) do
                            if RowSp[CurCol + k] = 0 then
                              inc(Pix, ColTextWidthMin[CurCol + k]);
                          if Pix < round(Width.LengthValue * MaxPercent / 100) then begin
                            Pix := (round(MaxPercent * Width.LengthValue / 100) - Pix)
                              div ColSpan;
                            for k := 0 to pred(ColSpan) do
                              if RowSp[CurCol + k] = 0 then
                                inc(ColTextWidthMin[CurCol + k], Pix);
                          end;
                        end;
                      end;
                      for k := 0 to pred(ColSpan) do begin
                        while RowSp[CurCol] <> 0 do begin
                          dec(RowSp[CurCol]);
                          inc(CurCol);
                        end;
                        RowSp[CurCol] := RowSpan - 1;
                        inc(CurCol);
                      end;
                    end;
                for j := CurCol to pred(ColCount) do
                  if RowSp[j] > 0 then
                    dec(RowSp[j]);
              end;
          end;
    for i := 0 to pred(ColCount) do
      RowSp[i] := 0;
    for z := 0 to pred(FChildren.Count) do
      if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
        with TIpHtmlNodeCore(FChildren[z]) do
          for i := 0 to pred(FChildren.Count) do begin
            if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
              with TIpHtmlNodeTR(FChildren[i]) do begin
                MaxPercent := 0;
                CurCol := 0;
                while RowSp[CurCol] <> 0 do begin
                  dec(RowSp[CurCol]);
                  inc(CurCol);
                end;
                for j := 0 to pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                      case Width.LengthType of
                      hlPercent :
                        begin
                          Pix := 0;
                          for k := 0 to pred(ColSpan) do
                            if RowSp[CurCol + k] = 0 then
                              inc(Pix, ColTextWidthMax[CurCol + k]);
                          Pix := round(100 * Pix / Width.LengthValue);
                          if Pix > MaxPercent then
                            MaxPercent := Pix;
                        end;
                      end;
                      for k := 0 to pred(ColSpan) do begin
                        while RowSp[CurCol] <> 0 do
                          inc(CurCol);
                        inc(CurCol);
                      end;
                    end;
                CurCol := 0;
              while RowSp[CurCol] <> 0 do begin
                dec(RowSp[CurCol]);
                inc(CurCol);
              end;
                for j := 0 to pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                      case Width.LengthType of
                      hlPercent :
                        if MaxPercent > 0 then begin
                          Pix := 0;
                          for k := 0 to pred(ColSpan) do
                            if RowSp[CurCol + k] = 0 then
                              inc(Pix, ColTextWidthMax[CurCol + k]);
                          if Pix < round(Width.LengthValue * MaxPercent / 100) then begin
                            Pix := (round(MaxPercent * Width.LengthValue / 100) - Pix)
                              div ColSpan;
                            for k := 0 to pred(ColSpan) do
                              if RowSp[CurCol + k] = 0 then
                                inc(ColTextWidthMax[CurCol + k], Pix);
                          end;
                        end;
                      end;
                      for k := 0 to pred(ColSpan) do begin
                        while RowSp[CurCol] <> 0 do begin
                          dec(RowSp[CurCol]);
                          inc(CurCol);
                        end;
                        RowSp[CurCol] := RowSpan - 1;
                        inc(CurCol);
                      end;
                    end;
                for j := CurCol to pred(ColCount) do
                  if RowSp[j] > 0 then
                    dec(RowSp[j]);
              end;
          end;
  end;
  *)

begin
  if FMin <> -1 then begin
    Min := FMin;
    Max := FMax;
    exit;
  end;

  FMin := 0;
  FMax := 0;
  if ColCount = 0 then
    exit;

  PendSpanWidthMin := nil;                                             {!!.10}
  PendSpanWidthMax := nil;                                             {!!.10}
  PendSpanStart := nil;                                                {!!.10}
  PendSpanSpan := nil;                                                 {!!.10}
  try                                                                  {!!.10}
    PendSpanWidthMin := TIntArr.Create;                                {!!.10}
    PendSpanWidthMax := TIntArr.Create;                                {!!.10}
    PendSpanStart := TIntArr.Create;                                   {!!.10}
    PendSpanSpan := TIntArr.Create;                                    {!!.10}

    {calc col and table widths}
    for i := 0 to pred(ColCount) do begin
      RowSp[i] := 0;
      ColTextWidthMin[i] := 0;
      ColTextWidthMax[i] := 0;
    end;
    PendCol := 0;
    for z := 0 to pred(FChildren.Count) do
      if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
        with TIpHtmlNodeCore(FChildren[z]) do
          for i := 0 to pred(FChildren.Count) do begin
            if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
              with TIpHtmlNodeTR(FChildren[i]) do begin
                CurCol := 0;
                while RowSp[CurCol] <> 0 do begin
                  {dec(RowSp[CurCol]);}                                  {!!.10}
                  RowSp[CurCol] := RowSp[CurCol] - 1;                    {!!.10}
                  inc(CurCol);
                end;
                for j := 0 to pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                      while RowSp[CurCol] <> 0 do begin                {!!.10}
                        RowSp[CurCol] := RowSp[CurCol] - 1;            {!!.10}
                        inc(CurCol);                                   {!!.10}
                      end;                                             {!!.10}

                      CalcMinMaxWidth(RenderProps, Min0, Max0);

                      case Width.LengthType of
                      hlAbsolute :
                        begin
                          if Width.LengthValue <= ExpParentWidth then  {!!.10}
                            Min0 := MaxI2(Min0, Width.LengthValue
                            {$IFDEF IP_LAZARUS}
                              - 2*CellPadding - CellSpacing - RUH);    {!!.10}
                            {$ELSE}
                              - 2*CellPadding - 2*CS2 - RUH);          {!!.10}
                            {$ENDIF}
                          Max0 := Min0;
                        end;
                      end;

                      FCalcWidthMin := Min0;                           {!!.10}
                      FCalcWidthMax := Max0;                           {!!.10}

                      DistributeColSpace(ColSpan);

                      for k := 0 to pred(ColSpan) do begin
                        while RowSp[CurCol] <> 0 do begin
                          {dec(RowSp[CurCol]);}                        {!!.10}
                          RowSp[CurCol] := RowSp[CurCol] - 1;          {!!.10}
                          inc(CurCol);
                        end;
                        RowSp[CurCol] := RowSpan - 1;
                        inc(CurCol);
                      end;
                    end;
                for j := CurCol to pred(ColCount) do
                  if RowSp[j] > 0 then
                    {dec(RowSp[j]);}                                   {!!.10}
                    RowSp[j] := RowSp[j] - 1;                          {!!.10}
              end;
          end;

      {BumpPercentages;} {!!.02}

      DistributeSpannedColSpace;
  finally
    PendSpanWidthMin.Free;
    PendSpanWidthMax.Free;
    PendSpanStart.Free;
    PendSpanSpan.Free;
  end;

  TWMin := 0;
  TWMax := 0;
  {$IFDEF IP_LAZARUS}
  CellOverhead := BL + CellSpacing + BR;
  {$ELSE}
  CellOverhead := BL + 2*CS2 + RUH + BR;
  {$ENDIF}
  for i := 0 to pred(ColCount) do begin
    inc(TWMin, ColTextWidthMin[i]);
    inc(TWMax, ColTextWidthMax[i]);
    {$IFDEF IP_LAZARUS}
    inc(CellOverhead, RUH + 2*CellPadding + CellSpacing + RUH);
    {$ELSE}
    inc(CellOverhead, 2*CellPadding + 2*CS2 + RUH);
    {$ENDIF}
    RowSp[i] := 0;
  end;

  FMin := MaxI2(FMin, TWMin + CellOverhead);
  FMax := MaxI2(FMax, TWMax + CellOverhead);
  Min := FMin;
  Max := FMax;
end;

procedure TIpHtmlNodeTABLE.SetRect(TargetRect: TRect);
var
  dx,dy : Integer;
  z, i, j : Integer;
  R : TRect;
begin
  if ColCount = 0 then exit;

  dx := TargetRect.Left - BorderRect2.Left;
  dy := TargetRect.Top - BorderRect2.Top;

  OffsetRect(BorderRect, dx, dy);
  OffsetRect(BorderRect2, dx, dy);
  if FCaption <> nil then begin
    with FCaption do begin
      if not IsRectEmpty(PageRect) then begin
        R := PageRect;
        OffsetRect(R, dx, dy);
        Layout(Props, R);
      end;
    end;
  end;

  for z := 0 to pred(FChildren.Count) do
    if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
      with TIpHtmlNodeCore(FChildren[z]) do
        for i := 0 to pred(FChildren.Count) do begin
          if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
            with TIpHtmlNodeTR(FChildren[i]) do begin

              for j := 0 to pred(FChildren.Count) do
                if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                  with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                    if not IsRectEmpty(PadRect) then
                      OffsetRect(FPadRect, dx, dy);
                    if not IsRectEmpty(PageRect) then begin
                      R := PageRect;
                      OffsetRect(R, dx, dy);
                      Layout(Props, R);
                    end;
                  end;
            end;
        end;
end;

procedure TIpHtmlNodeTABLE.CalcSize(const ParentWidth: Integer;
  const RenderProps: TIpHtmlProps);
{const}
  {MAXCOLS = 16384; 4096;}                                {!!.01}      {!!.10}
  {MAXSPANROWS = 16384;} {4096;}                          {!!.01}      {!!.10}
{type}                                                                 {!!.10}
  {TPRectArray = array[0..pred(MAXCOLS)] of PRect;
  PPRectArray = ^TPRectArray;}                                         {!!.10}
  (* !!.10
  TColPArr = record
    ColCount : Integer;
    Rects : TRectArr; {PPRectArray;}
  end;
  TRowSArr = array[0..pred(MaxSPANROWS)] of TColPArr;
  PRowSArr = ^TRowSArr;
  *)
var
  z, GrossCellSpace, NetCellSpace, CellExtra,
  NetCellSpaceExtraExtra,
  {maxY, maxYY,}       {moved into DoBlock}                            {!!.12}
  RelCellExtra,
  i, j, CurCol, k,
  {HA, HB, Y0,}        {moved into DoBlock}                            {!!.12}
  CellSpace,
  MinW, MaxW : Integer;
  {CellRect1 : TRect;} {moved into DoBlock}                            {!!.12}
  R : TRect;
  TargetRect : TRect;
  RowFixup : TRectRectArr; {PRowSArr;}
  RowFixupCount : Integer;
  {RowSp2 : TIntArr;}
  {VA0, VA : TIpHtmlVAlign3;}{moved into DoBlock}                      {!!.12}
  {AL0, AL : TIpHtmlAlign;} {moved into DoBlock}                       {!!.12}

  (*
   !!.10 no longer needed:
  procedure AddSpanRow(Cols: Integer);
  begin
    ReAllocMem(RowFixup, (RowFixupCount + 1) * sizeof(TColPArr));
    with RowFixup[RowFixupCount] do begin
      ColCount := Cols;
      Rects := AllocMem(Cols * sizeof(PRect));
    end;
    inc(RowFixupCount);
  end;

  procedure SetSpanRows(Rows, Cols: Integer);
  begin
    while RowFixupCount < Rows do
      AddSpanRow(Cols);
  end;
  *)

  function GetSpanBottom(Row, Col: Integer): Integer;
  var
    R: PRect;
  begin
    R := RowFixup.Value[Row].Value[Col];
    if R <> nil then
      Result := R.Bottom
    else
      Result := 0;
    (* !!.10 no longer needed:
    if Row < RowFixupCount then
      {if RowFixup[Row].Rects[Col] <> nil then}
        Result := RowFixup[Row].Rects.Rect[Col].Bottom
      {else
        Result := 0}
    else
      Result := 0;
    *)
  end;

  procedure SetSpanBottom(Row, Col, Value: Integer);
  var
    R: PRect;
  begin
    R := RowFixup.Value[Row].Value[Col];
    if R <> nil then
      R^.Bottom := Value;
    (* !!.10 no longer needed:
    if Row < RowFixupCount then
      {if RowFixup[Row].Rects[Col] <> nil then}
        RowFixup[Row].Rects.Rect[Col].Bottom := Value;
    *)
  end;

  procedure SetSpanRect(Row,Col : Integer; const Rect: PRect);
  begin
    RowFixup[Row].Value[Col] := Rect;                                  {!!.10}
    {RowFixup[Row].Rects[Col] := Rect^;}                               {!!.10}
  end;

  procedure DeleteFirstSpanRow;
  {var
    i : Integer;}
  begin
    RowFixup.Delete(0);
    (* !!.10 no longer needed:
    if RowFixup <> nil then begin
      Assert((RowFixupCount = 0) or not IsBadWritePtr(RowFixup[0].Rects, 4));
      RowFixup[0].Rects.Free;
      {if RowFixup[0].Rects <> nil then
        {FreeMem(RowFixup[0].Rects);}
      dec(RowFixupCount);
      for i := 0 to pred(RowFixupCount) do
        RowFixup[i] := RowFixup[i + 1];
      ReAllocMem(RowFixup, RowFixupCount * sizeof(TColPArr));
      {redundant:
      if RowFixupCount = 0 then begin
        FreeMem(RowFixup);
        RowFixup := nil;
      end;
      }
      Assert((RowFixupCount = 0) or not IsBadWritePtr(RowFixup[0].Rects, 4));
    end;
    *)
  end;

  (*
  procedure DeleteSpanArray;
  begin
    while RowFixup <> nil do
      DeleteFirstSpanRow;
  end;
  *)
  
  procedure AdjustCol(ColSpan, DesiredWidth: Integer);
  var
    i, Rest, WNow, Avail : Integer;
  begin
    WNow := 0;
    for i := CurCol to CurCol + ColSpan - 1 do
      inc(WNow, ColTextWidth[i]);
    Avail := MinI2(DesiredWidth, CellSpace);
    if WNow = 0 then begin
      for i := CurCol to CurCol + ColSpan - 1 do
         ColTextWidth[i] := Avail div ColSpan;
    end else begin
      Rest := MinI2(CellSpace, DesiredWidth - WNow);
      if Rest > 0 then begin
        for i := CurCol to CurCol + ColSpan - 1 do
           {inc(ColTextWidth[i],
             round(Rest * ColTextWidth[i] / WNow));}                   {!!.10}
          ColTextWidth[i] := ColTextWidth[i] +                         {!!.10}
            round(Rest * ColTextWidth[i] / WNow);                      {!!.10}
      end;
    end;
  end;

  procedure DoBlock(BlockType : TIpHtmlNodeTABLEHEADFOOTBODYClass);
  var
    z, i, j, k, zz : Integer;
    RowSp2 : TIntArr;
    AL0, AL : TIpHtmlAlign;                                            {!!.12}
    CellRect1 : TRect;                                                 {!!.12}
    HA, HB, Y0: Integer;                                               {!!.12}
    maxY, maxYY: Integer;                                              {!!.12}
    VA0, VA : TIpHtmlVAlign3;                                          {!!.12}
  begin
    RowSp2 := TIntArr.Create;                                          {!!.10}
    try
      for z := 0 to pred(FChildren.Count) do
        if (TIpHtmlNode(FChildren[z]) is BlockType) then
          with TIpHtmlNodeCore(FChildren[z]) do
            for i := 0 to pred(FChildren.Count) do begin
              if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
                with TIpHtmlNodeTR(FChildren[i]) do begin

                  for j := 0 to pred(ColCount) do
                    RowSp2[j] := RowSp[j];

                  CurCol := 0;
                  while RowSp[CurCol] <> 0 do begin
                    {dec(RowSp[CurCol]);}                                {!!.10}
                    RowSp[CurCol] := RowSp[CurCol] - 1;                  {!!.10}
                    inc(CurCol);
                  end;

                  VA0 := Props.VAlignment;
                  case VAlign of
                  hvaTop :
                    VA0 := hva3Top;
                  hvaMiddle :
                    VA0 := hva3Middle;
                  hvaBottom :
                    VA0 := hva3Bottom;
                  end;

                  case Align of
                  haDefault :
                    AL0 := haLeft;
                  else
                    AL0 := Align;
                  end;

                  {determine height of cells and lay out with top alignment}
                  for j := 0 to pred(FChildren.Count) do
                    if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                      with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin
                        while RowSp[CurCol] <> 0 do begin
                          {dec(RowSp[CurCol]);}                          {!!.10}
                          RowSp[CurCol] := RowSp[CurCol] - 1;            {!!.10}
                          inc(CurCol);
                        end;

                        AL := AL0;

                        Props.Assign(Self.Props);

                        CellRect1 := TargetRect;

                        inc(CellRect1.Left,
                          ColStart[CurCol]);

                        {$IFDEF IP_LAZARUS}
                        inc(CellRect1.Top, CellSpacing + RUV);
                        {$ELSE}
                        inc(CellRect1.Top, CS2 + RUV);
                        {$ENDIF}

                        CellRect1.Right :=
                          CellRect1.Left
                          + 2*CellPadding
                          + ColTextWidth[CurCol]
                          {$IFDEF IP_LAZARUS}
                          ;
                          {$ELSE}
                          + 2*CS2;
                          {$ENDIF}

                        for k := 1 to ColSpan - 1 do
                          inc(CellRect1.Right,
                            ColTextWidth[CurCol + k] +
                            2*CellPadding +
                            {$IFDEF IP_LAZARUS}
                            2*RUH +
                            CellSpacing);
                            {$ELSE}
                            2*CS2 + RUH);
                            {$ENDIF}
                            
                        {$IFDEF IP_LAZARUS}
                        // PadRect area of cell excluding rules
                        // CellRect area of text contained in cell
                        FPadRect := CellRect1;
                        inc(CellRect1.Top, CellPadding);
                        inflateRect(CellRect1, -CellPadding, 0);
                        {$ELSE}
                        FPadRect := CellRect1;
                        InflateRect(FPadRect, -CS2, 0);

                        inc(CellRect1.Top, CellPadding);
                        InflateRect(CellRect1, -(CellPadding + CS2), 0);
                        {$ENDIF}

                        VA := VAlign;
                        if VA = hva3Default then
                          VA := VA0;

                        case Align of
                        haDefault : ;
                        else
                          AL := Align;
                        end;

                        Props.VAlignment := VA;
                        Props.Alignment := AL;
                        Layout(Props, CellRect1);

                        {SetSpanRows(MaxI2(RowSpan, RowFixupCount + 1), ColCount);} {!!.10}

                        if (Height.PixelsType <> hpUndefined) {Height <> -1} then {!!.10}
                          if PageRect.Bottom - PageRect.Top < Height.Value then   {!!.10}
                            FPageRect.Bottom := CellRect1.Top + Height.Value;     {!!.10}

                        if (Height.PixelsType = hpUndefined) {Height = -1}        {!!.10}
                        and IsRectEmpty(PageRect) then
                          FPadRect.Bottom := CellRect1.Top + CellPadding
                        else begin
                          FPadRect.Bottom := PageRect.Bottom + CellPadding;
                        end;
                        SetSpanRect(RowSpan - 1, CurCol, @PadRect);

                        for k := 0 to pred(ColSpan) do begin
                          RowSp[CurCol] := RowSpan - 1;
                          inc(CurCol);
                        end;
                      end;

                  {Adjust any trailing spanning columns}
                  for j := CurCol to pred(ColCount) do
                    if RowSp[j] > 0 then
                      {dec(RowSp[j]);}                                   {!!.10}
                      RowSp[j] := RowSp[j] - 1;                          {!!.10}

                  maxYY := 0;
                  maxY := 0;
                  {if RowFixupCount > 0 then begin}
                    for zz := 0 to pred(ColCount) do
                      maxY := MaxI2(GetSpanBottom(0, zz), maxY);
                    for zz := 0 to pred(ColCount) do
                      SetSpanBottom(0, zz, maxY);
                   if maxY > maxYY then
                     maxYY := maxY;
                  {end;}

                  for j := 0 to pred(ColCount) do
                    RowSp[j] := RowSp2[j];

                  CurCol := 0;
                  while RowSp[CurCol] <> 0 do begin
                    {dec(RowSp[CurCol]);}                                {!!.10}
                    RowSp[CurCol] := RowSp[CurCol] - 1;                  {!!.10}
                    inc(CurCol);
                  end;
                  {relocate cells which are not top aligned}
                  for j := 0 to pred(FChildren.Count) do
                    if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                      with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin
                        while RowSp[CurCol] <> 0 do begin
                          {dec(RowSp[CurCol]);}                          {!!.10}
                          RowSp[CurCol] := RowSp[CurCol] - 1;            {!!.10}
                          inc(CurCol);
                        end;

                        AL := AL0;

                        {$IFDEF IP_LAZARUS}
                        HA := maxYY - (TargetRect.Top + CellSpacing + RUV);
                        {$ELSE}
                        HA := maxYY - TargetRect.Top;
                        {$ENDIF}
                        HB := PageRect.Bottom - PageRect.Top;

                        VA := VAlign;
                        if VA = hva3Default then
                          VA := VA0;

                        case VA of
                        hva3Middle :
                          Y0 := (HA - HB) div 2;
                        hva3Bottom :
                          Y0 := (HA - HB);
                        else
                          Y0 := 0;
                        end;

                        if Y0 > 0 then begin

                          CellRect1 := TargetRect;

                          inc(CellRect1.Left,
                            ColStart[CurCol]);

                          {$IFDEF IP_LAZARUS}
                          inc(CellRect1.Top, CellSpacing + RUV + Y0);
                          {$ELSE}
                          inc(CellRect1.Top, CS2 + RUV + Y0);

                          {$ENDIF}
                          CellRect1.Right :=
                            CellRect1.Left
                            + 2*CellPadding
                            + ColTextWidth[CurCol]
                            {$IFDEF IP_LAZARUS}
                            ;
                            {$ELSE}
                            + 2*CS2;
                            {$ENDIF}

                          for k := 1 to ColSpan - 1 do
                            inc(CellRect1.Right,
                              ColTextWidth[CurCol + k] +
                              2*CellPadding +
                              {$IFDEF IP_LAZARUS}
                              2*RUH + CellSpacing);
                              {$ELSE}
                              2*CS2 + RUH);
                              {$ENDIF}

                          inc(CellRect1.Top, CellPadding);
                          {$IFDEF IP_LAZARUS}
                          inflateRect(CellRect1, -CellPadding, 0);
                          {$ELSE}
                          InflateRect(CellRect1, -(CellPadding + CS2), 0);
                          {$ENDIF}

                          case Align of
                          haDefault : ;
                          else
                            AL := Align;
                          end;

                          Props.VAlignment := VA;
                          Props.Alignment := AL;

                          Layout(Props, CellRect1);

                          {SetSpanRows(MaxI2(RowSpan, RowFixupCount + 1), ColCount);} {!!.10}

                          if Height.PixelsType <> hpUndefined {Height <> -1} then {!!.10}
                            if PageRect.Bottom - PageRect.Top < Height.Value then {!!.10}
                              FPageRect.Bottom := CellRect1.Top + Height.Value;   {!!.10}

                          if (Height.PixelsType = hpUndefined) {(Height = -1)}    {!!.10}
                          and IsRectEmpty(PageRect) then
                            FPadRect.Bottom := CellRect1.Top + CellPadding
                          else begin
                            FPadRect.Bottom := PageRect.Bottom + CellPadding;
                          end;
                          SetSpanRect(RowSpan - 1, CurCol, @PadRect);

                        end;

                        for k := 0 to pred(ColSpan) do begin
                          RowSp[CurCol] := RowSpan - 1;
                          inc(CurCol);
                        end;
                      end;

                  maxYY := 0;
                  maxY := 0;

                  {if RowFixupCount > 0 then begin}
                    for zz := 0 to pred(ColCount) do
                      maxY := MaxI2(GetSpanBottom(0, zz), maxY);
                    for zz := 0 to pred(ColCount) do
                      SetSpanBottom(0, zz, maxY);
                    if maxY > maxYY then
                      maxYY := maxY;
                  {end;}

                  {Adjust any trailing spanning columns}
                  for j := CurCol to pred(ColCount) do
                    if RowSp[j] > 0 then
                      {dec(RowSp[j]);}                                   {!!.10}
                      RowSp[j] := RowSp[j] - 1;                          {!!.10}    

                  {$IFDEF IP_LAZARUS}
                  TargetRect.Top := MaxI2(maxYY, TargetRect.Top) + RUV;
                  {$ELSE}
                  TargetRect.Top := MaxI2(maxYY, TargetRect.Top);

                  {$ENDIF}
                  DeleteFirstSpanRow;
                end;
            end;

      while RowFixupCount > 0  do begin
        maxYY := 0;
        maxY := 0;
        for zz := 0 to pred(ColCount) do
          maxY := MaxI2(GetSpanBottom(0, zz), maxY);
        for zz := 0 to pred(ColCount) do
          SetSpanBottom(0, zz, maxY);
        if maxY > maxYY then
          maxYY := maxY;

        TargetRect.Top := MaxI2(maxYY, TargetRect.Top);

        DeleteFirstSpanRow;
      end;

    finally
      RowSp2.Free;
    end;
  end;

var
  P : Integer;
  {Red : double;}
begin

  FTableWidth := 0;

  if ColCount = 0 then
    exit;

  Props.Assign(RenderProps);

  CalcMinMaxColTableWidth(Props, MinW, MaxW);

  case Width.LengthType of
  hlUndefined :
    begin
      P := 0;
      for z := 0 to pred(FChildren.Count) do
        if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
          with TIpHtmlNodeCore(FChildren[z]) do
            for i := 0 to pred(FChildren.Count) do begin
              if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
                with TIpHtmlNodeTR(FChildren[i]) do begin
                  for j := 0 to pred(FChildren.Count) do
                    if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                      with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                        case Width.LengthType of
                        hlPercent :
                          inc(P, Width.LengthValue);
                        end;
                      end;
                end;
            end;
      if P <> 0 then
        FTableWidth := MaxI2(MinW, round((P * ParentWidth) / 100))      {!!.10}
      else
        FTableWidth := MaxI2(MinW, MinI2(MaxW, ParentWidth));
    end;
  hlAbsolute :
    FTableWidth :=
      MaxI2(Width.LengthValue, MinW);
  hlPercent :
    FTableWidth := MaxI2(MinW,                                          {!!.10}
      round(
         (Width.LengthValue * ParentWidth) / 100));
  end;

  (* !!.13
  if FTableWidth >= MaxW then begin
    for i := 0 to pred(ColCount) do
      ColTextWidth[i] := ColTextWidthMin[i];
  end else begin
    {if TableWidth < MinW then begin
      Red := TableWidth / MinW;
      for i := 0 to pred(ColCount) do begin
        ColTextWidthMin[i] := round(Red * ColTextWidthMin[i]);
        ColTextWidth[i] := ColTextWidthMin[i];
      end;
    end else}
      for i := 0 to pred(ColCount) do
        ColTextWidth[i] := ColTextWidthMin[i];
  end;
  *)
  for i := 0 to pred(ColCount) do                                    {!!.13}
    ColTextWidth[i] := ColTextWidthMin[i];                           {!!.13}

  for z := 0 to pred(ColCount) do
    RowSp[z] := 0;

  for z := 0 to pred(FChildren.Count) do
    if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
      with TIpHtmlNodeCore(FChildren[z]) do
        for i := 0 to pred(FChildren.Count) do begin
          if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
            with TIpHtmlNodeTR(FChildren[i]) do begin

              CellSpace := FTableWidth - CellOverhead;
              for j := 0 to pred(ColCount) do
                dec(CellSpace, ColTextWidth[j]);

              if CellSpace > 0 then begin
                {distribute extra space}
                CurCol := 0;
                while RowSp[CurCol] <> 0 do begin
                  {dec(RowSp[CurCol]);}                                {!!.10}
                  RowSp[CurCol] := RowSp[CurCol] - 1;                  {!!.10} 
                  inc(CurCol);
                end;
                for j := 0 to pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                      case Width.LengthType of
                      hlAbsolute :
                        AdjustCol(ColSpan, Width.LengthValue -
                        {$IFDEF IP_LAZARUS}
                          2*CellPadding - CellSpacing - RUH);
                        {$ELSE}
                          2*CellPadding - 2*CS2 - RUH);
                        {$ENDIF}
                      hlPercent :
                        AdjustCol(Colspan,
                                  round((FTableWidth - CellOverhead) *
                                        Width.LengthValue / 100));
                      end;

                      CellSpace := FTableWidth - CellOverhead;
                      for k := 0 to pred(ColCount) do
                        dec(CellSpace, ColTextWidth[k]);

                      for k := 0 to pred(ColSpan) do begin
                        while RowSp[CurCol] <> 0 do begin
                          {dec(RowSp[CurCol]);}                        {!!.10}
                          RowSp[CurCol] := RowSp[CurCol] - 1;          {!!.10}
                          inc(CurCol);
                        end;
                        RowSp[CurCol] := RowSpan - 1;
                        inc(CurCol);
                      end;
                    end;
                for j := CurCol to pred(ColCount) do
                  if RowSp[j] > 0 then
                    {dec(RowSp[j]);}                                   {!!.10}
                    RowSp[j] := RowSp[j] - 1;                          {!!.10}
              end;
            end;
        end;

  GrossCellSpace := MaxI2(FTableWidth - CellOverhead, 0);
  NetCellSpace := 0;
  for i := 0 to pred(ColCount) do
    inc(NetCellSpace, ColTextWidth[i]);
  if NetCellSpace > 0 then begin
    CellExtra := GrossCellSpace - NetCellSpace;
    if CellExtra > 0 then
      for i := 0 to pred(ColCount) do begin
        RelCellExtra := round(CellExtra / NetCellSpace * ColTextWidth[i] );
        if ColTextWidth[i] + RelCellExtra > ColTextWidthMax[i] then
          ColTextWidth[i] := MaxI2(ColTextWidth[i], ColTextWidthMax[i])
        else
          ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
      end;
  end;

  NetCellSpace := 0;
  for i := 0 to pred(ColCount) do
    inc(NetCellSpace, ColTextWidth[i]);
  CellExtra := GrossCellSpace - NetCellSpace;
  if CellExtra > 0 then begin
    RelCellExtra := CellExtra div ColCount;
    NetCellSpaceExtraExtra := CellExtra mod ColCount;
    for i := 0 to pred(ColCount) do begin
      if (ColTextWidth[i] < ColTextWidthMax[i]) then begin
        ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
        if NetCellSpaceExtraExtra > 0 then begin
          {inc(ColTextWidth[i]);}                                      {!!.10}
          ColTextWidth[i] := ColTextWidth[i] + 1;                      {!!.10}
          dec(NetCellSpaceExtraExtra);
        end;
      end;
    end;
  end;
  NetCellSpace := 0;
  for i := 0 to pred(ColCount) do
    inc(NetCellSpace, ColTextWidth[i]);
  CellExtra := GrossCellSpace - NetCellSpace;
  if CellExtra > 0 then begin
    for i := 0 to pred(ColCount) do begin
      RelCellExtra := MinI2(ColTextWidthMax[i] - ColTextWidth[i], CellExtra);
      if RelCellExtra > 0 then begin
        {inc(ColTextWidth[i], RelCellExtra);}                          {!!.10}
        ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;             {!!.10}
        dec(CellExtra, RelCellExtra);
      end;
    end;
  end;
  NetCellSpace := 0;
  for i := 0 to pred(ColCount) do
    inc(NetCellSpace, ColTextWidth[i]);
  CellExtra := GrossCellSpace - NetCellSpace;
  if CellExtra > 0 then begin
    RelCellExtra := CellExtra div ColCount;
    NetCellSpaceExtraExtra := CellExtra mod ColCount;
    for i := 0 to pred(ColCount) do begin
      ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
      if NetCellSpaceExtraExtra > 0 then begin
        {inc(ColTextWidth[i]);}                                        {!!.10}
        ColTextWidth[i] := ColTextWidth[i] + 1;                        {!!.10}
        dec(NetCellSpaceExtraExtra);
      end;
    end;
  end;

  for i := 0 to pred(ColCount) do
    RowSp[i] := 0;

  TargetRect := Rect(0, 0, ParentWidth, MaxInt);

  BorderRect2 := TargetRect;
  BorderRect := TargetRect;

  for z := 0 to pred(FChildren.Count) do
    if TIpHtmlNode(FChildren[z]) is TIpHtmlNodeCAPTION then begin
      FCaption := TIpHtmlNodeCAPTION(FChildren[z]);
      if FCaption.Align <> hva2Bottom then begin
        FCaption.Layout(Props, BorderRect2);
        inc(BorderRect.Top, FCaption.PageRect.Bottom - FCaption.PageRect.Top);
      end;
    end;

  TargetRect := BorderRect;

  R := BorderRect;

  {$IFDEF IP_LAZARUS}
  ColStart[0] := BL + CellSpacing + RUH;
  {$ELSE}
  ColStart[0] := BL + CS2 + RUH;
  {$ENDIF}
  RowSp[0] := 0;
  for i := 1 to pred(ColCount) do begin
    ColStart[i] :=
      ColStart[i-1]
      + 2*CellPadding
      + ColTextWidth[i-1]
      {$IFDEF IP_LAZARUS}
      + CellSpacing
      + 2*RUH;
      {$ELSE}
      + 2*CS2
      + RUH;
      {$ENDIF}
    RowSp[i] := 0;
  end;

  {calc size of table body}

  inc(TargetRect.Top, BT);

  {calc rows}

  RowFixup := TRectRectArr.Create;
  try
    RowFixupCount := 0;

    DoBlock(TIpHtmlNodeTHEAD);
    DoBlock(TIpHtmlNodeTBODY);
    DoBlock(TIpHtmlNodeTFOOT);

    {DeleteSpanArray;}                                                 {!!.10}

    {if RowFixup <> nil then
      FreeMem(RowFixup);}                                              {!!.10}
  finally
    RowFixup.Free;
  end;
  
  {$IFDEF IP_LAZARUS}
  inc(TargetRect.Top, CellSpacing + RUV + BB);
  {$ELSE}
  inc(TargetRect.Top, CS2 + RUV + BB);
  {$ENDIF}

  R.Right := R.Left + FTableWidth;
  R.Bottom := TargetRect.Top;

  if (R.Bottom > R.Top) and (R.Right = R.Left) then
    R.Right := R.Left + 1;

  BorderRect.BottomRight := R.BottomRight;
  BorderRect2.BottomRight := R.BottomRight;

  if assigned(FCaption) and (FCaption.Align = hva2Bottom) then begin
    R.Top := BorderRect.Bottom;
    R.Bottom := MaxInt;
    FCaption.Layout(Props, R);
    BorderRect2.Bottom := FCaption.PageRect.Bottom;
  end;
end;

constructor TIpHtmlNodeTABLE.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  BgColor := -1;
  SizeWidth := TIpHtmlPixels.Create;
  SizeWidth.PixelsType := hpUndefined;
  FColCount := -1;
  FMin := -1;
  FMax := -1;
  ColTextWidth := TIntArr.Create;
  ColStart := TIntArr.Create;
  ColTextWidthMin := TIntArr.Create;
  ColTextWidthMax := TIntArr.Create;
  RowSp := TIntArr.Create;
end;

procedure TIpHtmlNodeTABLE.Draw;
var
  z, i, j : Integer;
  R : TRect;
  Al : TIpHtmlVAlign3;
begin
  if (BGColor <> -1) and PageRectToScreen(BorderRect, R) then begin
    Owner.Target.Brush.Color := BGColor;
    Owner.Target.FillRect(R);
  end;

  Owner.Target.Pen.Color := clBlack;

  Al := Props.VAlignment;

  for z := 0 to pred(ColCount) do
    RowSp[z] := 0;

  for z := 0 to pred(FChildren.Count) do
    if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
      with TIpHtmlNodeCore(FChildren[z]) do
        for i := 0 to pred(FChildren.Count) do begin
          if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
            with TIpHtmlNodeTR(FChildren[i]) do begin

              case VAlign of
              hvaTop :
                Al := hva3Top;
              hvaMiddle :
                Al := hva3Middle;
              hvaBottom :
                Al := hva3Bottom;
              end;

              for j := 0 to pred(FChildren.Count) do
                if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                  with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do begin

                    case VAlign of
                    hva3Default :
                      ;
                    else
                      Al := VAlign;
                    end;

                    Props.VAlignment := Al;

                    Render(Props);

                    {paint left rule if selected}
                    case Rules of
                    hrNone,
                    hrGroups :;
                    hrRows :;
                    hrCols,
                    hrAll :
                      begin
                        if not IsRectEmpty(PadRect) then begin
                          R := PadRect;
                          Inflaterect(R, 1, 1);
                          {$IFDEF IP_LAZARUS}
                          ScreenFrame(R, False);
                          {$ELSE}
                          ScreenRect(R, RGB(192,192,192));
                          {$ENDIF}
                        end;
                      end;
                    end;

                  end;

            end;
        end;

  {render frames}
  if Frame in [hfAbove, hfHSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        BorderRect.TopLeft,
        Point(BorderRect.Right-1, BorderRect.Top),
        1,
        RGB(220,220,220))
    else
      ScreenPolygon(
        [BorderRect.TopLeft,
        Point(BorderRect.Right, BorderRect.Top),
        Point(BorderRect.Right - (Border - 1), BorderRect.Top + Border - 1),
        Point(BorderRect.Left + Border - 1, BorderRect.Top + Border - 1)],
        RGB(220,220,220));
  if Frame in [hfBelow, hfHSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
        Point(BorderRect.Left, BorderRect.Bottom - 1),
        1,
        RGB(64,64,64))
    else
    ScreenPolygon(
      [
      Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
      Point(BorderRect.Right - (Border - 1), BorderRect.Bottom - (Border - 1) - 1),
      Point(BorderRect.Left + Border, BorderRect.Bottom - (Border - 1) - 1),
      Point(BorderRect.Left, BorderRect.Bottom - 1)],
        RGB(64,64,64));
  if Frame in [hfLhs, hfvSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        BorderRect.TopLeft,
        Point(BorderRect.Left, BorderRect.Bottom - 1),
        1,
        RGB(192,192,192))
    else
      ScreenPolygon(
        [BorderRect.TopLeft,
        Point(BorderRect.Left, BorderRect.Bottom - 1),
        Point(BorderRect.Left + (Border - 1), BorderRect.Bottom - Border),
        Point(BorderRect.Left + (Border - 1), BorderRect.Top + (Border - 1))],
        RGB(192,192,192));
  if Frame in [hfRhs, hfvSides, hfBox, hfBorder] then
    if Border = 1 then
      ScreenLine(
        Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
        Point(BorderRect.Right - 1, BorderRect.Top),
        1,
        RGB(128,128,128))
    else
      ScreenPolygon(
        [
        Point(BorderRect.Right - 1, BorderRect.Bottom - 1),
        Point(BorderRect.Right - 1, BorderRect.Top),
        Point(BorderRect.Right - (Border - 1) - 1, BorderRect.Top + (Border - 1)),
        Point(BorderRect.Right - (Border - 1) - 1, BorderRect.Bottom - Border)],
        RGB(128,128,128));

  {render caption}
  if assigned(FCaption) then
    FCaption.Render(Props);
end;

procedure TIpHtmlNodeTABLE.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.NoBreak := False;
  inherited SetProps(RenderProps);                                     {!!.10}
end;

function TIpHtmlNodeTABLE.GetDim(ParentWidth: Integer): TSize;
begin
  if (SizeWidth.PixelsType <> hpAbsolute)
  or (SizeWidth.Value <> ParentWidth) then begin
    SizeWidth.PixelsType := hpUndefined;
    CalcSize(ParentWidth, Props);
    SizeWidth.Value := ParentWidth;
    SizeWidth.PixelsType := hpAbsolute;
  end;
  Result :=
    SizeRec(BorderRect2.Right - BorderRect2.Left,
      BorderRect2.Bottom - BorderRect2.Top);
end;

procedure TIpHtmlNodeTABLE.CalcMinMaxWidth(var Min, Max: Integer);
begin
  CalcMinMaxColTableWidth(Props, Min, Max);
  case Width.LengthType of
  hlAbsolute :
    begin
      Min := MaxI2(Min, Width.LengthValue);
      Max := MaxI2(Max, Min);
    end;
  end;
end;

procedure TIpHtmlNodeTABLE.InvalidateSize;
begin
  SizeWidth.PixelsType := hpUndefined;
  FMin := -1;
  FMax := -1;
  inherited;
end;

function TIpHtmlNodeTABLE.GetColCount: Integer;
var
  z, i, j, c : Integer;
begin
  if FColCount = -1 then begin
    FColCount := 0;
    for z := 0 to pred(FChildren.Count) do
      if (TIpHtmlNode(FChildren[z]) is TIpHtmlNodeTHeadFootBody) then
        with TIpHtmlNodeCore(FChildren[z]) do
          for i := 0 to pred(FChildren.Count) do begin
            c := 0;
            if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeTR then
              with TIpHtmlNodeTR(FChildren[i]) do
                for j := 0 to pred(FChildren.Count) do
                  if TIpHtmlNode(FChildren[j]) is TIpHtmlNodeTableHeaderOrCell then
                    with TIpHtmlNodeTableHeaderOrCell(FChildren[j]) do
                      inc(c, Colspan);
            if c > FColCount then
              FColCount := c;
          end;
    {$IFNDEF IP_LAZARUS}
    CS2 := CellSpacing div 2;
    if (CellSpacing > 0) and (CS2 = 0) then
      CS2 := 1;
    {$ENDIF}
    RUH := 0;
    RUV := 0;
    case Rules of
    hrNone :;
    hrGroups :
      begin
        RUH := 1;
        RUV := 1;
      end;
    hrRows :
      RUV := 1;
    hrCols :
      RUH := 1;
    hrAll :
      begin
        RUH := 1;
        RUV := 1;
      end;
    end;
    BL := 0; BR := 0;
    BT := 0; BB := 0;
    case Frame of
    hfVoid,
    hfAbove :
      BT := Border;
    hfBelow :
      BB := Border;
    hfHSides :
      begin
        BT := Border;
        BB := Border;
      end;
    hfLhs :
      BL := Border;
    hfRhs :
      BR := Border;
    hfvSides :
      begin
        BL := Border;
        BR := Border;
      end;
    hfBox,
    hfBorder :
      begin
        BT := Border;
        BB := Border;
        BL := Border;
        BR := Border;
      end;
    end;
  end;
  Result := FColCount;
end;

procedure TIpHtmlNodeTABLE.Enqueue;
begin
  case Align of
  hiaTop,
  hiaMiddle,
  hiaBottom,
  hiaCenter :
    EnqueueElement(Owner.SoftLF);
  end;
  EnqueueElement(Element);
  case Align of
  hiaTop,
  hiaMiddle,
  hiaBottom,
  hiaCenter :
    EnqueueElement(Owner.SoftLF);
  end;
end;

procedure TIpHtmlNodeTABLE.SetBorder(const Value: Integer);
begin
  FBorder := Value;
  if Border = 0 then begin
    Frame := hfVoid;
    Rules := hrNone;
  end else begin
    Frame := hfBorder;
    Rules := hrAll;
  end;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetCellPadding(const Value: Integer);
begin
  FCellPadding := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetCellSpacing(const Value: Integer);
begin
  FCellSpacing := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetFrame(const Value: TIpHtmlFrameProp);
begin
  FFrame := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeTABLE.SetRules(const Value: TIpHtmlRules);
begin
  FRules := Value;
  InvalidateSize;
end;

destructor TIpHtmlNodeTABLE.Destroy;
begin
  inherited;
  FWidth.Free;
  SizeWidth.Free;
  ColTextWidth.Free;
  ColStart.Free;
  ColTextWidthMin.Free;
  ColTextWidthMax.Free;
  RowSp.Free;
end;

procedure TIpHtmlNodeTABLE.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

function TIpHtmlNodeTABLE.ExpParentWidth: Integer;
begin
  case Width.LengthType of
  hlAbsolute :
    Result := Width.LengthValue;
  else
    Result := inherited ExpParentWidth;
  end;
end;

{ TIpHtmlNodeTR }

constructor TIpHtmlNodeTR.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FAlign := haDefault;
  FValign := hvaMiddle;
end;

{ TIpHtmlNodeMAP }

constructor TIpHtmlNodeMAP.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  Owner.MapList.Add(Self);
end;

destructor TIpHtmlNodeMAP.Destroy;
begin
  Owner.MapList.Remove(Self);
  inherited;
end;

{ TIpHtmlNodeAREA }

destructor TIpHtmlNodeAREA.Destroy;
var
  I: Integer;
begin
  I := Owner.AreaList.IndexOf(Self);
  if I <> -1 then
    Owner.AreaList.Delete(I);
  inherited;
end;

function TIpHtmlNodeAREA.GetHint: string;
begin
  if Alt <> '' then
    Result := Alt
  else
    Result := HRef;
end;

function TIpHtmlNodeAREA.PtInRects(const P: TPoint): Boolean;
begin
  if PtInRect(FRect, P) then
    Result := True
  else
  if FRgn <> 0 then
    Result := PtInRegion(FRgn, P.x, P.y)
  else
    Result := False;
end;

procedure TIpHtmlNodeAREA.Reset;
begin
  if FRgn <> 0 then
    DeleteObject(FRgn);
  SetRectEmpty(FRect);
end;

{ TIpHtmlNodeIMG }

procedure TIpHtmlNodeIMG.LoadImage;
{var !!.10 no longer used
  ScaledImage : TPicture;
  ScaledBmp : TBitmap;}
begin
  if Src <> '' then begin
    if FPicture <> Owner.DefaultImage then begin                       {!!.10}
      FPicture.Free;                                                   {!!.10}
      FPicture := nil;                                                 {!!.10}
    end;                                                               {!!.10}
    Owner.DoGetImage(Self, Owner.BuildPath(Src), FPicture);
    if FPicture = nil
      then FPicture := Owner.DefaultImage;

    (* !!.10 no longer used
    if ScaleBitmaps then begin                                         {!!.02}
      ScaledImage := TPicture.Create;                                  {!!.02}
      ScaledBmp := TBitmap.Create;                                     {!!.02}
      ScaledBmp.Width := round(FPicture.Width * Aspect);               {!!.02}
      ScaledBmp.Height := round(FPicture.Height * Aspect);             {!!.02}
      ScaledImage.Graphic := ScaledBmp;                                {!!.02}
      ScaledImage.Bitmap.Canvas.StretchDraw(                           {!!.02}
        Rect(0, 0, ScaledBmp.Width - 1, ScaledBmp.Height - 1),         {!!.02}
        FPicture.Graphic);                                             {!!.02}
      ScaledBmp.Free;                                                  {!!.10}
      FPicture.Free;                                                   {!!.02}
      FPicture := ScaledImage;                                         {!!.02}
    end;                                                               {!!.02}
    *)

    {$IFDEF UseGifImageUnit}
    if (FPicture <> nil)
    and (FPicture.Graphic <> nil)
    and (FPicture.Graphic is TGifImage)
    then
      Owner.GifImages.Add(Self);
    {$ELSE}
    if (FPicture <> nil)
    and (FPicture.Graphic <> nil)
    and (FPicture.Graphic is TIpAnimatedGraphic)
    then
      Owner.AnimationFrames.Add(Self);
    {$ENDIF}
  end;
end;

{!!.02 new - logic moved here from .Destroy}
procedure TIpHtmlNodeIMG.UnloadImage;
begin
  {$IFDEF UseGifImageUnit}
  if (FPicture <> nil)
  and (FPicture.Graphic <> nil)
  and (FPicture.Graphic is TGifImage)
  then
    Owner.GifImages.Remove(Self);
  {$ELSE}
  if (FPicture <> nil)
  and (FPicture.Graphic <> nil)
  and (FPicture.Graphic is TIpAnimatedGraphic)
  then
    Owner.AnimationFrames.Remove(Self);
  {$ENDIF}
  if FPicture <> Owner.DefaultImage then begin
    FPicture.Free;
    FPicture := nil;
  end;
end;

destructor TIpHtmlNodeIMG.Destroy;
begin
  UnloadImage;                                                         {!!.02}
  UseMap := '';
  inherited;
  FWidth.Free;                                                         {!!.10}
  SizeWidth.Free;                                                      {!!.10}
  FHeight.Free;                                                        {!!.10}
end;

{$IFDEF IP_LAZARUS}
function TIpHtmlNodeIMG.GetBorder: integer;
begin
  if (FPicture<>nil)and(FPicture.Graphic=nil) then
    result := 1
  else
    result := fBorder;
end;
{$ENDIF}
procedure TIpHtmlNodeIMG.Draw;
var
  R : TRect;
  TopLeft : TPoint;
  Dim : TSize;
begin
  if FPicture = nil then
    LoadImage;

  if (FPicture <> nil) and (FPicture.Graphic = nil) then               {!!.15}
    LoadImage;
  Owner.AddRect(GrossDrawRect, Element, Block);
  TopLeft := GrossDrawRect.TopLeft;
  R.TopLeft := TopLeft;
  Dim := GetDim(0);
  R.Right := TopLeft.x + Dim.cx;
  R.Bottom := TopLeft.y + Dim.cy;
  
  if Border <> 0 then begin
    if Border = 1 then begin
      ScreenLine(
        R.TopLeft,
        Point(R.Right, R.Top),
        1,
        RGB(220,220,220));
      ScreenLine(
        R.BottomRight,
        Point(R.Left, R.Bottom),
        1,
        RGB(64,64,64));
      ScreenLine(
        R.TopLeft,
        Point(R.Left, R.Bottom),
        1,
        RGB(192,192,192));
      ScreenLine(
        R.BottomRight,
        Point(R.Right, R.Top),
        1,
        RGB(128,128,128));
    end else begin
      ScreenPolygon(
        [R.TopLeft,
        Point(R.Right - 1, R.Top),
        Point(R.Right - Border, R.Top + Border - 1),
        Point(R.Left + Border - 1, R.Top + Border - 1)],
        RGB(220,220,220));
      ScreenPolygon(
        [
        Point(R.Right - 1, R.Bottom - 1),
        Point(R.Right - Border, R.Bottom - Border),
        Point(R.Left + (Border - 1), R.Bottom - Border),
        Point(R.Left, R.Bottom - 1)],
          RGB(64,64,64));
      ScreenPolygon(
        [R.TopLeft,
        Point(R.Left, R.Bottom - 1),
        Point(R.Left + (Border - 1), R.Bottom - Border),
        Point(R.Left + (Border - 1), R.Top + (Border - 1))],
        RGB(192,192,192));
      ScreenPolygon(
        [
        Point(R.Right - 1, R.Bottom - 1),
        Point(R.Right - 1, R.Top),
        Point(R.Right - Border, R.Top + (Border - 1)),
        Point(R.Right - Border, R.Bottom - Border)],
        RGB(128,128,128));
    end;
    InflateRect(R, -Border, -Border);
  end;

  InflateRect(R, -HSpace, -VSpace);

  if FPicture <> nil then begin
  {$IFDEF IP_LAZARUS}
    if FPicture.Graphic=nil then begin
      if PageRectToScreen(R,R) then
        Owner.Target.TextRect(R, R.Left, R.Top, GetHint);
      exit;
    end;
  {$ENDIF}
    FPicture.Graphic.Transparent := True;
    NetDrawRect := R;
    if PageRectToScreen(R, R) then begin
      {$IFDEF UseGifImageUnit}
      if (FPicture.Graphic is TGifImage)
      and (TGifImage(FPicture.Graphic).Images.Count > 1) then begin
        TGifImage(FPicture.Graphic).DrawOptions :=
          TGifImage(FPicture.Graphic).DrawOptions + [goDirectDraw];
        Owner.AddGifQueue(FPicture.Graphic, R);
      end else
      {$ELSE}
      if (FPicture.Graphic is TIpAnimatedGraphic)
      and (TIpAnimatedGraphic(FPicture.Graphic).Images.Count > 1) then begin
        TIpAnimatedGraphic(FPicture.Graphic).AggressiveDrawing := True;
        Owner.AddGifQueue(FPicture.Graphic, R);
      end else
      {$ENDIF}
      begin
        if FPicture = Owner.DefaultImage then begin
          if ((NetDrawRect.Right - NetDrawRect.Left) > FPicture.Graphic.Width)
          and ((NetDrawRect.Bottom - NetDrawRect.Top) > FPicture.Graphic.Height) then begin
            Owner.Target.Brush.Color := Props.FontColor;
            Owner.Target.FrameRect(R);
            Owner.Target.Draw(R.Left + 1, R.Top + 1, FPicture.Graphic);
          end else
            Owner.Target.StretchDraw(R, FPicture.Graphic);
        end else
          Owner.Target.StretchDraw(R, FPicture.Graphic);
      end;
    end;
  end
end;

function TIpHtmlNodeIMG.GrossDrawRect : TRect;
begin
  Result := PIpHtmlElement(Element).WordRect2;
end;

procedure TIpHtmlNodeIMG.ReportDrawRects(M: TRectMethod);
begin
  M(GrossDrawRect);
end;

procedure TIpHtmlNodeIMG.ReportMapRects(M: TRectMethod);
begin
  if IsMap then
    M(GrossDrawRect);
end;

procedure TIpHtmlNodeIMG.ImageChange(NewPicture: TPicture);
var
  OldDim,
  Dim : TSize;
begin
  {$IFOPT C+}
  Owner.CheckImage(NewPicture);
  {$ENDIF}
  OldDim := GetDim(-1);
  {$IFDEF UseGifImageUnit}
  if (FPicture <> nil)
  and (FPicture.Graphic <> nil)
  and (FPicture.Graphic is TGifImage)
  then
    Owner.GifImages.Remove(Self);
  {$ELSE}
  if (FPicture <> nil)
  and (FPicture.Graphic <> nil)
  and (FPicture.Graphic is TIpAnimatedGraphic)
  then
    Owner.AnimationFrames.Remove(Self);
  {$ENDIF}
  if FPicture <> Owner.DefaultImage then
    FPicture.Free;
  FPicture := NewPicture;
  {$IFDEF UseGifImageUnit}
  if (FPicture <> nil)
  and (FPicture.Graphic <> nil)
  and (FPicture.Graphic is TGifImage)
  then
    Owner.GifImages.Add(Self);
  {$ELSE}
  if (FPicture <> nil)
  and (FPicture.Graphic <> nil)
  and (FPicture.Graphic is TIpAnimatedGraphic)
  then
    Owner.AnimationFrames.Add(Self);
  {$ENDIF}
  SizeWidth.PixelsType := hpUndefined;
  Dim := GetDim(0);
  if (Dim.cx <> OldDim.cx)
  or (Dim.cy <> OldDim.cy) then
    InvalidateSize
  else
    Invalidate;
end;

procedure TIpHtmlNodeIMG.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
end;

function TIpHtmlNodeIMG.GetDim(ParentWidth: Integer): TSize;
var
  DimKnown, NoLoad : Boolean;
begin
  if ParentWidth < 0 then begin
    NoLoad := True;
    ParentWidth := 0;
  end else
    NoLoad := False;
  if (SizeWidth.PixelsType <> hpAbsolute)
  or ((ParentWidth <> 0) and (SizeWidth.Value <> ParentWidth)) then begin
    DimKnown := True;
    if (Height.PixelsType <> hpUndefined) {(Height > -1)}              {!!.10}
    and (Width.LengthType <> hlUndefined) then begin
      case Width.LengthType of
      hlUndefined :
        DimKnown := False;
      hlAbsolute :
        begin
          FSize := SizeRec(Width.LengthValue, Height.Value);
        end;
      hlPercent :
        begin
          FSize := SizeRec(
            round(ParentWidth * Width.LengthValue / 100)
               - 2*HSpace - 2*Border,                                  {!!.10}
            Height.Value);                                             {!!.10}
        end;
      end;
    end else
      DimKnown := False;
    if not DimKnown then begin
      if (FPicture <> nil) then begin
        {$IFDEF IP_LAZARUS}
        if FPicture.Graphic=nil then
          // todo: needs to return the "text size" of GetHint
          FSize := SizeRec(100,20)
        else
        {$ENDIF}
        if ScaleBitmaps then                                         {!!.10}
          FSize := SizeRec(round(FPicture.Width * Aspect), round(FPicture.Height * Aspect))
        else
          FSize := SizeRec(FPicture.Width, FPicture.Height)
      end else begin
        if NoLoad then
          FSize := SizeRec(0, 0)
        else begin
          LoadImage;
          if FPicture <> nil then begin
            if ScaleBitmaps then                                         {!!.10}
              FSize := SizeRec(round(FPicture.Width * Aspect), round(FPicture.Height * Aspect))
            else
              {$IFDEF IP_LAZARUS}
              if FPicture.Graphic=nil then
                // todo: needs to return the "text size" of GetHint
                FSize := SizeRec(100,20)
              else
              {$ENDIF}
              FSize := SizeRec(FPicture.Width, FPicture.Height);
          end else
            FSize := SizeRec(0, 0);
        end;
      end;
      if FPicture <> nil then begin
        case Width.LengthType of
        hlUndefined :;
        hlAbsolute :
          begin
            FSize := SizeRec(Width.LengthValue, FSize.cy);
          end;
        hlPercent :
          begin
            FSize := SizeRec(
              round(ParentWidth * Width.LengthValue / 100)
               - 2*HSpace - 2*Border,                                  {!!.10}
              FSize.cy);
          end;
        end;
        if Height.PixelsType <> hpUndefined {Height <> -1} then        {!!.10}
          FSize.cy := Height.Value;                                    {!!.10}
      end;
    end;
    FSize := SizeRec(FSize.cx + 2*HSpace + 2*Border, FSize.cy + 2*VSpace + 2*Border);
    SizeWidth.Value := ParentWidth;
    SizeWidth.PixelsType := hpAbsolute;
  end;
  Result := FSize;
end;

procedure TIpHtmlNodeIMG.CalcMinMaxWidth(var Min, Max: Integer);
var
  Dim : TSize;
begin
  Dim := GetDim(0);
  Min := Dim.cx;
  Max := Min;
end;

procedure TIpHtmlNodeIMG.SetUseMap(const Value: string);
begin
  if FUseMap <> '' then begin
    Owner.MapImgList.Remove(Self);
    Owner.ClearAreaList;
  end;
  FUseMap := Value;
  if FUseMap <> '' then begin
    Owner.MapImgList.Add(Self);
    Owner.ClearAreaList;
  end;
end;

function TIpHtmlNodeIMG.GetHint: string;
begin
  Result := Alt;
end;

procedure TIpHtmlNodeIMG.SetBorder(const Value: Integer);
begin
  FBorder := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.SetHSpace(const Value: Integer);
begin
  FHSpace := Value;
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.SetVSpace(const Value: Integer);
begin
  FVSpace := Value;
  InvalidateSize;
end;

{!!.10 new}
constructor TIpHtmlNodeIMG.Create;
begin
  inherited;
  SizeWidth := TIpHtmlPixels.Create;
end;

procedure TIpHtmlNodeIMG.DimChanged(Sender: TObject);
begin
  InvalidateSize;
end;

procedure TIpHtmlNodeIMG.InvalidateSize;
begin
  inherited;
  SizeWidth.PixelsType := hpUndefined;
end;

{ TIpHtmlNodeFORM }

constructor TIpHtmlNodeFORM.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Props := TIpHtmlProps.Create(Owner);
end;

destructor TIpHtmlNodeFORM.Destroy;
begin
  Props.Free;
  inherited;
end;

procedure TIpHtmlNodeFORM.AddChild(Node: TIpHtmlNode; const UserData: Pointer);
begin
  if Node is TIpHtmlNodeControl then
    if TIpHtmlNodeControl(Node).SuccessFul then
      TList(UserData).Add(Node);
end;

{$IFNDEF HtmlWithoutHttp}
procedure TIpHtmlNodeFORM.SubmitForm;
var
  CList : TList;
  FList,
  VList : TStringList;
  URLData: string;
  FormData: TIpFormDataEntity;

  procedure IndentifySuccessfulControls;
  begin
    EnumChildren(AddChild, CList);
  end;

  procedure BuildDataset;
  var
    i : Integer;
  begin
    for i := 0 to pred(CList.Count) do
      with TIpHtmlNodeControl(CList[i]) do
        AddValues(FList, VList);
  end;

  procedure URLEncodeDataset;

    function Escape(const S: string): string;
    var
      i : Integer;
    begin
      Result := '';
      for i := 1 to length(S) do
        case S[i] of
        #0..#31, '+', '&', '%', '=' :
          Result := Result + '%'+IntToHex(ord(S[i]),2);
        ' ' :
          Result := Result + '+';
        else
          Result := Result + S[i];
        end;
    end;

  var
    i : Integer;
  begin
    URLData := '';
    for i := 0 to pred(FList.Count) do begin
      if URLData <> '' then
        URLData := URLData + '&';
      URLData := URLData +
        Escape(FList[i]) +
        '=' +
        Escape(VList[i]);
    end;
  end;

  procedure MimeEncodeDataset;
  var
    i : Integer;
  begin
    FormData := TIpFormDataEntity.Create(nil);
    for i := 0 to pred(FList.Count) do
      if copy(VList[i], 1, 7) = 'file://' then
        FormData.AddFile(copy(VList[i], 8, length(VList[i])),
          Accept, 'plain', embinary)
      else
        FormData.AddFormData(FList[i], VList[i]);
  end;

  procedure SubmitDataset;
  begin
    case Method of
    hfmGet :
      Owner.Get(Action + '?' + URLData);
    hfmPost :
      begin
        Owner.Post(Action, FormData); {!!.12}
        {The Formdata object will be freed by the post logic,
         which is called asynchroneously via PostMessage.
         Clear the pointer to prevent our finalization
         section from stepping on it prematurely.}
        FormData := nil; {!!.12}
      end;
    end;
  end;

begin
  FormData := nil;
  CList := nil;
  FList := nil;
  VList := nil;
  try
    CList := TList.Create;
    FList := TStringList.Create;
    VList := TStringList.Create;
    IndentifySuccessfulControls;
    BuildDataset;
    case Method of  {!!.12}
    hfmGet :        {!!.12}
    {if (EncType = '') or
      (CompareText(EncType, 'application/x-www-form-urlencoded') = 0) then} {!!.12}
      URLEncodeDataset;
    else //hfmPost :       {!!.12}
    {else
    if CompareText(EncType, 'multipart/form-data') = 0 then} {!!.12}
      MimeEncodeDataset;
    end; {!!.12}
    {else
      raise EIpHtmlException.Create(EncType + SHtmlEncNotSupported);} {!!.02} {!!.12}
    SubmitDataset;
  finally
    FormData.Free;
    CList.Free;
    FList.Free;
    VList.Free;
  end;
end;

procedure TIpHtmlNodeFORM.SubmitRequest;
begin
  SubmitForm;
end;
{$ENDIF}

procedure TIpHtmlNodeFORM.ResetRequest;
begin
  ResetForm;
end;

procedure TIpHtmlNodeFORM.ResetControl(Node: TIpHtmlNode; const UserData: Pointer);
begin
  if Node is TIpHtmlNodeControl then
    TIpHtmlNodeControl(Node).Reset;
end;

procedure TIpHtmlNodeFORM.ResetForm;
begin
  EnumChildren(ResetControl, nil);
end;

{ TIpHtmlNodeDL }

{!!.16 new}
procedure TIpHtmlNodeDL.Enqueue;
begin
  EnqueueElement(Owner.HardLF);
  EnqueueElement(Owner.LIndent);
  inherited;
  EnqueueElement(Owner.LOutdent);
end;

{ TIpHtmlNodeDT }

procedure TIpHtmlNodeDT.Enqueue;
begin
  inherited;
  EnqueueElement(Owner.HardLF);
end;

{ TIpHtmlNodeDD }

procedure TIpHtmlNodeDD.Enqueue;
begin
  EnqueueElement(Owner.HardLF);                                        {!!.16}
  EnqueueElement(Owner.LIndent);
  inherited;
  EnqueueElement(Owner.LOutdent);
  EnqueueElement(Owner.HardLF);
end;

{ TIpHtmlNodePRE }

constructor TIpHtmlNodePRE.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Props := TIpHtmlProps.Create(Owner);
end;

destructor TIpHtmlNodePRE.Destroy;
begin
  Props.Free;
  inherited;
end;

procedure TIpHtmlNodePRE.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.Preformatted := True;
  Props.FontName := Owner.FixedTypeface;
  Props.FontSize := Props.FontSize - 2;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodePRE.Enqueue;
begin
  if FChildren.Count > 0 then
    EnqueueElement(Owner.HardLF);
  inherited Enqueue;
  if FChildren.Count > 0 then
    EnqueueElement(Owner.HardLF);
end;

{ TIpHtmlNodeBLOCKQUOTE }

procedure TIpHtmlNodeBLOCKQUOTE.Enqueue;
begin
  EnqueueElement(Owner.LIndent);
  inherited;
  EnqueueElement(Owner.LOutdent);
end;

{ TIpHtmlNodePhrase }

procedure TIpHtmlNodePhrase.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  case Style of
  hpsEM :
    Props.FontStyle := Props.FontStyle + [fsItalic];
  hpsSTRONG :
    Props.FontStyle := Props.FontStyle + [fsBold];
  hpsCODE :
    Props.FontName := Owner.FixedTypeface;
  hpsKBD :
    Props.FontName := Owner.FixedTypeface;
  hpsVAR :
    Props.FontStyle := Props.FontStyle + [fsItalic];
  hpsCITE :
    Props.FontStyle := Props.FontStyle + [fsItalic];
  end;
end;

{ TIpHtmlNodeAPPLET }

{!!.10 new}
destructor TIpHtmlNodeAPPLET.Destroy;
begin
  inherited;
  FWidth.Free;
end;

function TIpHtmlNodeAPPLET.GetHint: string;
begin
  Result := Alt;
end;

procedure TIpHtmlNodeAPPLET.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeBASEFONT }

procedure TIpHtmlNodeBASEFONT.ApplyProps(
  const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  case Size of
  1 : Props.FontSize := 8;
  2 : Props.FontSize := 10;
  3 : Props.FontSize := 12;
  4 : Props.FontSize := 14;
  5 : Props.FontSize := 18;
  6 : Props.FontSize := 24;
  7 : Props.FontSize := 36;
  end;
  Props.BaseFontSize := Size;
end;

{ TIpHtmlNodeINPUT }

procedure TIpHtmlNodeINPUT.SetImageGlyph(Picture: TPicture);
var
  FBitmap : TBitmap;
begin
  with TBitbtn(FControl) do begin
    FBitmap := TBitmap.Create;
    try
      FBitmap.Width := Picture.Width;
      FBitmap.Height := Picture.Height;
      Picture.Graphic.Transparent := False;
      FBitmap.TransparentMode := tmFixed;
      FBitmap.TransparentColor := RGB(254, 254, 254);
      FBitmap.Canvas.Draw(0, 0, Picture.Graphic);
      Glyph.Assign(FBitmap);
      Width := FBitmap.Width + 4;
      Height := FBitmap.Height + 4;
    finally
      FBitmap.Free;
    end;
  end;
end;

procedure TIpHtmlNodeINPUT.Reset;
begin
  case InputType of
  hitText :
    begin
      with TEdit(FControl) do
        Text := Value;
    end;
  hitPassword :
    begin
      with TEdit(FControl) do
        Text := Value;
    end;
  hitCheckbox :
    begin
      with TCheckBox(FControl) do
        Checked := Self.Checked;
    end;
  hitRadio :
    begin
{Begin !!.14}
{$IFDEF VERSION3ONLY}
      with FControl do
{$ELSE}
      with THtmlRadioButton(FControl) do
{$ENDIF}
{End !!.14}
        Checked := Self.Checked;
    end;
  end;
end;

procedure TIpHtmlNodeINPUT.CreateControl(Parent: TWinControl);

  function OwnerForm: TIpHtmlNode;
  begin
    Result := FParentNode;
    while (Result <> nil) and not (Result is TIpHtmlNodeFORM) do
      Result := Result.FParentNode;
  end;

begin
  Owner.ControlCreate(Self);
  case InputType of
  hitText :
    begin
      FControl := TEdit.Create(Parent);
      FControl.Visible := False;
      FControl.Parent := Parent;
      with TEdit(FControl) do begin
        Text := Value;
        MaxLength := Self.MaxLength;
        if Self.Size <> -1 then
          Width := Self.Size * TFriendPanel(Parent).Canvas.TextWidth('0')
        else
          Width := 20 * TFriendPanel(Parent).Canvas.TextWidth('0');
        Enabled := not Self.Disabled;
        ReadOnly := Self.ReadOnly;
        OnChange := ButtonClick; {!!.03}
      end;
    end;
  hitPassword :
    begin
      FControl := TEdit.Create(Parent);
      FControl.Visible := False;
      FControl.Parent := Parent;
      with TEdit(FControl) do begin
        Text := Value;
        MaxLength := Self.MaxLength;
        Width := Self.Size * TFriendPanel(Parent).Canvas.TextWidth('0');
        Enabled := not Self.Disabled;
        ReadOnly := Self.ReadOnly;
        PasswordChar := '*';
        OnChange := ButtonClick; {!!.03}
      end;
    end;
  hitCheckbox :
    begin
      FControl := TCheckBox.Create(Parent);
      FControl.Visible := False;
      FControl.Parent := Parent;
      with TCheckBox(FControl) do begin
        Width := 13;
        Height := 13;
        Checked := Self.Checked;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ButtonClick;
      end;
    end;
  hitRadio :
    begin
{Begin !!.14}
{$IFDEF VERSION3ONLY}
      FControl := TRadioButton.Create(Parent);
{$ELSE}
      FControl := THtmlRadioButton.Create(Parent);
{$ENDIF}
      FControl.Tag := PtrInt(OwnerForm);
      FControl.Visible := False;
      FControl.Parent := Parent;
{$IFDEF VERSION3ONLY}
      with TRadioButton(FControl) do begin
{$ELSE}
      with THtmlRadioButton(FControl) do begin
{$ENDIF}
{End !!.14}
        Width := 13;
        Height := 13;
        Checked := Self.Checked;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ButtonClick;
      end;
    end;
  hitSubmit :
    begin
      FControl := TButton.Create(Parent);
      FControl.Visible := False;
      FControl.Parent := Parent;
      with TButton(FControl) do begin
        if Self.Value <> '' then
          Caption := Self.Value
        else
          Caption := SHtmlDefSubmitCaption;
        Width := TFriendPanel(Parent).Canvas.TextWidth(Caption) + 40;
        Height := TFriendPanel(Parent).Canvas.TextHeight(Caption) + 10;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := SubmitClick;
      end;
    end;
  hitReset :
    begin
      FControl := TButton.Create(Parent);
      FControl.Visible := False;
      FControl.Parent := Parent;
      with TButton(FControl) do begin
        if Self.Value <> '' then
          Caption := Self.Value
        else
          Caption := SHtmlDefResetCaption;
        Width := TFriendPanel(Parent).Canvas.TextWidth(Caption) + 40;
        Height := TFriendPanel(Parent).Canvas.TextHeight(Caption) + 10;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ResetClick;
      end;
    end;
  hitFile :
    begin
      FControl := TPanel.Create(Parent);
      FControl.Visible := False;
      FControl.Parent := Parent;
      with TPanel(FControl) do begin
        Width := 200;
        Height := TFriendPanel(Parent).Canvas.TextHeight('Wy') + 12;
        Enabled := not Self.Disabled and not Self.Readonly;
        BevelInner := bvNone;
        BevelOuter := bvNone;
        BorderStyle := bsNone;
      end;
      FFileSelect := TButton.Create(Parent);
      with FFileSelect do begin
        Parent := FControl;
        Height := TFriendPanel(Parent).Canvas.TextHeight(SHtmlDefBrowseCaption) + 10;
        Width := TFriendPanel(Parent).Canvas.TextWidth(SHtmlDefBrowseCaption) + 40;
        Left := FControl.Left + FControl.Width - Width;
        Top := 1;
        Caption := SHtmlDefBrowseCaption;
        OnClick := FileSelect;
      end;
      FFileEdit := TEdit.Create(Parent);
      with FFileEdit do begin
        Parent := FControl;
        Left := 1;
        Top := 1;
        Width := FControl.Width - FFileSelect.Width;
        Height := FControl.Height - 2;
      end;
    end;
  hitHidden :
    begin
    end;
  hitImage :
    begin
      FControl := TBitbtn.Create(Parent);
      FControl.Visible := False;
      FControl.Parent := Parent;
      Owner.DoGetImage(Self, Owner.BuildPath(Src), FPicture);
      if FPicture = nil
        then FPicture := Owner.DefaultImage;
      with TBitbtn(FControl) do begin
        Caption := Self.Value;
        Enabled := not Self.Disabled and not Self.Readonly;
        SetImageGlyph(FPicture);
      end;
    end;
  hitButton :
    begin
      FControl := TButton.Create(Parent);
      FControl.Visible := False;
      FControl.Parent := Parent;
      with TButton(FControl) do begin
        Caption := Self.Value;
        Width := TFriendPanel(Parent).Canvas.TextWidth(Caption) + 40;
        Height := TFriendPanel(Parent).Canvas.TextHeight(Caption) + 10;
        Enabled := not Self.Disabled and not Self.Readonly;
        OnClick := ButtonClick;
      end;
    end;
  end;
  if FControl <> nil then
    FControl.Hint := Alt;
end;

procedure TIpHtmlNodeINPUT.Draw;
begin
  inherited;
{Begin !!.14}
{$IFDEF VERSION3ONLY}
  if FControl is TRadioButton then begin
{$ELSE}
  if FControl is THtmlRadioButton then begin
{$ENDIF}
    if Props.BgColor <> -1 then
{$IFDEF VERSION3ONLY}
      TRadioButton(FControl).Color := Props.BgColor;
{$ELSE}
      THtmlRadioButton(FControl).Color := Props.BgColor;
{$ENDIF}
{End !!.14}
  end;
end;

procedure TIpHtmlNodeINPUT.ImageChange(NewPicture: TPicture);
begin
  {$IFOPT C+}
  Owner.CheckImage(NewPicture);
  {$ENDIF}
  if FPicture <> Owner.DefaultImage then
    FPicture.Free;
  FPicture := NewPicture;
  SetImageGlyph(FPicture);
  InvalidateSize;
end;

procedure TIpHtmlNodeINPUT.AddValues(NameList, ValueList : TStringList);
var
  S : string;
begin
  S := '';
  case InputType of
  hitText,
  hitPassword :
    S := TEdit(FControl).Text;
  hitCheckbox :
    S := Value;
  hitRadio :
    S := Value;
  hitFile :
    S := 'file://'+FFileEdit.Text;
  hitHidden :                                                          {!!.15}
    S := FValue;                                                       {!!.15}
  end;
  if S <> '' then begin
    NameList.Add(Name);
    ValueList.Add(S);
  end;
end;

function TIpHtmlNodeINPUT.Successful: Boolean;
begin
{Begin !!.15}
  Result :=
    (Name <> '')and
    ( (InputType = hitHidden) or
      ( (not Disabled) and
        (InputType in [hitText, hitPassword, hitCheckbox, hitRadio , hitFile])
      )
    );
{End !!.15}
  if Result then begin
    case InputType of
    hitText,
    hitPassword :
      Result := TEdit(FControl).Text <> '';
    hitCheckbox :
      Result := TCheckBox(FControl).Checked;
    hitRadio :
{$IFDEF VERSION3ONLY}
      Result := TRadioButton(FControl).Checked;
{$ELSE}
      Result := THtmlRadioButton(FControl).Checked;
{$ENDIF}
    hitFile :
      Result := FFileEdit.Text <> '';
    hitHidden :                                                        {!!.15}
      Result := FValue <> '';                                          {!!.15}
    end;
  end;
end;

procedure TIpHtmlNodeINPUT.SubmitClick(Sender: TObject);
begin
  SubmitRequest;
end;

procedure TIpHtmlNodeINPUT.ResetClick(Sender: TObject);
begin
  ResetRequest;
end;

procedure TIpHtmlNodeINPUT.ButtonClick(Sender: TObject);
begin
  case InputType of
  hitText,                                                             {!!.03}
  hitPassword :                                                        {!!.03}
    Value := TEdit(FControl).Text;                                     {!!.03}
  hitCheckbox :
    Checked := TCheckBox(FControl).Checked;
  hitRadio :
{$IFDEF VERSION3ONLY}
    Checked := TRadioButton(FControl).Checked;
{$ELSE}
    Checked := THtmlRadioButton(FControl).Checked;
{$ENDIF}
  end;
  Owner.ControlClick(Self);
end;

function TIpHtmlNodeINPUT.GetHint: string;
begin
  Result := Alt;
end;

procedure TIpHtmlNodeINPUT.FileSelect(Sender: TObject);
begin
  with TOpenDialog.Create(FControl) do
    try
      if Execute then
        FFileEdit.Text := FileName;
    finally
      free;
    end;
end;

destructor TIpHtmlNodeINPUT.Destroy;
begin
  inherited;
  FPicture.Free;
end;

{ TIpHtmlNodeSELECT }

procedure TIpHtmlNodeSELECT.AddValues(NameList, ValueList : TStringList);
var
  i : Integer;
begin
  if FControl is TListBox then
    with TListBox(FControl) do begin
      for i := 0 to pred(Items.Count) do
        if Selected[i] then begin
          NameList.Add(Self.Name);
          ValueList.Add(Items[i]);
        end;
    end
  else with TComboBox(FControl) do begin
    NameList.Add(Self.Name);
    ValueList.Add(Items[ItemIndex]);
  end;
end;

procedure TIpHtmlNodeSELECT.CreateControl(Parent: TWinControl);
var
  i, j, k, MinW : Integer;
  S, SelectedText : string;
  B : PAnsiChar;
begin
  Owner.ControlCreate(Self);
  if Self.Multiple then begin
    FControl := TListBox.Create(Parent);
    FControl.Visible := False;
    FControl.Parent := Parent;
    with TListBox(FControl) do begin
      IntegralHeight := True;
      Height := (4 + ItemHeight) * Self.Size;
      MultiSelect := True;
      Enabled := not Self.Disabled;
      OnClick := ButtonClick;                                          {!!.01}
    end;
  end else begin
    FControl := TComboBox.Create(Parent);
    FControl.Visible := False;
    FControl.Parent := Parent;
    with TComboBox(FControl) do begin
      Style := csDropDownList;
      Height := (4 + ItemHeight) * Self.Size;
      Enabled := not Self.Disabled;
      OnClick := ButtonClick;                                          {!!.01}
    end;
  end;
  MinW := 50;
  SelectedText := '';
  for i := 0 to pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeOPTION then
      with TIpHtmlNodeOPTION(FChildren[i]) do begin
        if (FChildren.Count > 0)
        and (TObject(FChildren[0]) is TIpHtmlNodeText) then begin
          S := TIpHtmlNodeText(FChildren[0]).EscapedText;
          Getmem(B, length(S) + 1);
          try
            TrimFormattingNormal(S, B);
            if Self.Multiple then begin
              j := TListBox(FControl).Items.Add(Trim(B));
              MinW := MaxI2(MinW, TFriendPanel(Parent).Canvas.TextWidth(Trim(B)));
              TListBox(FControl).Selected[j] := Selected;
            end else begin
              TComboBox(FControl).Items.Add(Trim(B));
              MinW := MaxI2(MinW, TFriendPanel(Parent).Canvas.TextWidth(Trim(B)));
              if Selected then
                SelectedText := Trim(B);
            end;
          finally
            FreeMem(B);
          end;
        end;
      end
    else
    if TObject(FChildren[i]) is TIpHtmlNodeOPTGROUP then
      with TIpHtmlNodeOPTGROUP(FChildren[i]) do begin
        for j := 0 to pred(FChildren.Count) do
          if TObject(FChildren[j]) is TIpHtmlNodeOPTION then
            with TIpHtmlNodeOPTION(FChildren[j]) do begin
              if (FChildren.Count > 0)
              and (TObject(FChildren[0]) is TIpHtmlNodeText) then begin
                S := TIpHtmlNodeText(FChildren[0]).EscapedText;
                GetMem(B, length(S) + 1);
                try
                  TrimFormattingNormal(S, B);
                  if Self.Multiple then begin
                    k := TListBox(FControl).Items.Add(Trim(B));
                    MinW := MaxI2(MinW, TFriendPanel(Parent).Canvas.TextWidth(Trim(B)));
                    TListBox(FControl).Selected[k] := Selected;
                  end else begin
                    TComboBox(FControl).Items.Add(Trim(B));
                    MinW := MaxI2(MinW, TFriendPanel(Parent).Canvas.TextWidth(Trim(B)));
                    if Selected then
                      SelectedText := Trim(B);
                  end;
                finally
                  FreeMem(B);
                end;
              end;
            end;
      end;
  if SelectedText <> '' then
    with TComboBox(FControl) do
      ItemIndex := Items.IndexOf(SelectedText);
  FControl.Width := MinW + 40;
end;

procedure TIpHtmlNodeSELECT.Reset;
var
  i, j, k : Integer;
  S, SelectedText : string;
  B : PAnsiChar;
begin
  SelectedText := '';
  if Self.Multiple then
    TListBox(FControl).Clear
  else
    TComboBox(FControl).Clear;
  for i := 0 to pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeOPTION then
      with TIpHtmlNodeOPTION(FChildren[i]) do begin
        if (FChildren.Count > 0)
        and (TObject(FChildren[0]) is TIpHtmlNodeText) then begin
          S := TIpHtmlNodeText(FChildren[0]).EscapedText;
          GetMem(B, length(S) + 1);
          try
            TrimFormattingNormal(S, B);
            if Self.Multiple then begin
              j := TListBox(FControl).Items.Add(Trim(B));
              TListBox(FControl).Selected[j] := Selected;
            end else begin
              TComboBox(FControl).Items.Add(Trim(B));
              if Selected then
                SelectedText := Trim(B);
            end;
          finally
            FreeMem(B);
          end;
        end;
      end
    else
    if TObject(FChildren[i]) is TIpHtmlNodeOPTGROUP then
      with TIpHtmlNodeOPTGROUP(FChildren[i]) do begin
        for j := 0 to pred(FChildren.Count) do
          if TObject(FChildren[j]) is TIpHtmlNodeOPTION then
            with TIpHtmlNodeOPTION(FChildren[j]) do begin
              if (FChildren.Count > 0)
              and (TObject(FChildren[0]) is TIpHtmlNodeText) then begin
                S := TIpHtmlNodeText(FChildren[0]).EscapedText;
                GetMem(B, length(S) + 1);
                try
                  TrimFormattingNormal(S, B);
                  if Self.Multiple then begin
                    k := TListBox(FControl).Items.Add(Trim(B));
                    TListBox(FControl).Selected[k] := Selected;
                  end else begin
                    TComboBox(FControl).Items.Add(Trim(B));
                    if Selected then
                      SelectedText := Trim(B);
                  end;
                finally
                  FreeMem(B);
                end;
              end;
            end;
      end;
  if not Self.Multiple and (SelectedText <> '') then
    with TComboBox(FControl) do
      ItemIndex := Items.IndexOf(SelectedText);
end;

function TIpHtmlNodeSELECT.Successful: Boolean;
begin
  Result :=
    (Name <> '')
    and not Disabled;
  if Result then
    if FControl is TListBox then begin
      Result := TListBox(FControl).SelCount > 0;
    end
    else begin
      Result := TComboBox(FControl).ItemIndex <> -1;
    end;
end;

{!!.01 - added}
procedure TIpHtmlNodeSELECT.ButtonClick(Sender: TObject);
begin
  Owner.ControlClick(Self);
end;

{ TIpHtmlNodeTEXTAREA }

procedure TIpHtmlNodeTEXTAREA.AddValues(NameList,
  ValueList: TStringList);
begin
  NameList.Add(Name);
  ValueList.Add(TMemo(FControl).Text);
end;

procedure TIpHtmlNodeTEXTAREA.CreateControl(Parent: TWinControl);
var
  i : Integer;
  S : string;
  B : PAnsiChar;
begin
  Owner.ControlCreate(Self);
  FControl := TMemo.Create(Parent);
  FControl.Visible := False;
  FControl.Parent := Parent;
  with TMemo(FControl) do begin
    Width := Cols * TFriendPanel(Parent).Canvas.TextWidth('0'); 
    Height := Rows * TFriendPanel(Parent).Canvas.TextHeight('Wy');
    Enabled := not Self.Disabled;
  end;
  for i := 0 to pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeText then begin
      S := TIpHtmlNodeText(FChildren[i]).EscapedText;
      Getmem(B, length(S) + 1);
      try
        TrimFormattingNormal(S, B);
        TMemo(FControl).Lines.Add(B);
      finally
        FreeMem(B);
      end;
    end;
end;

procedure TIpHtmlNodeTEXTAREA.Reset;
var
  i : Integer;
  S : string;
  B : PAnsiChar;
begin
  TMemo(FControl).Clear;
  for i := 0 to pred(FChildren.Count) do
    if TObject(FChildren[i]) is TIpHtmlNodeText then begin
      S := TIpHtmlNodeText(FChildren[i]).EscapedText;
      GetMem(B, length(S) + 1);
      try
        TrimFormattingNormal(S, B);
        TMemo(FControl).Lines.Add(B);
      finally
        Freemem(B);
      end;
    end;
end;

function TIpHtmlNodeTEXTAREA.Successful: Boolean;
begin
  Result := trim(TMemo(FControl).Text) <> '';
end;

{ TIpHtmlNodeHtml }

procedure TIpHtmlNodeHtml.CalcMinMaxWidth(
  const RenderProps: TIpHtmlProps; var Min,
  Max: Integer);
var
  i : Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then begin
      TIpHtmlNodeBody(FChildren[i]).CalcMinMaxWidth(RenderProps,
        Min, Max);
    end;
end;

function TIpHtmlNodeHtml.GetHeight(const RenderProps: TIpHtmlProps;
  const Width: Integer): Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then begin
      Result := TIpHtmlNodeBody(FChildren[i]).
        GetHeight(RenderProps, Width);
    end;
end;

{Begin !!.12}
function TIpHtmlNodeHtml.HasBodyNode : Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to FChildren.Count - 1 do begin
    Result := (TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody);
    if Result then
      Break;
  end;
end;
{End !!.12}

procedure TIpHtmlNodeHtml.Layout(const RenderProps: TIpHtmlProps;
  const TargetRect: TRect);
var
  i : Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      TIpHtmlNodeBody(FChildren[i]).Layout(
        RenderProps, TargetRect);
end;

procedure TIpHtmlNodeHtml.Render(const RenderProps: TIpHtmlProps);
var
  i : Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      TIpHtmlNodeBody(FChildren[i]).
        Render(RenderProps);
end;

{ TIpHtmlNodeCore }

procedure TIpHtmlNodeCore.ParseBaseProps;
begin
  with Owner do begin
    Id := FindAttribute('ID');
    ClassId := FindAttribute('CLASS');
    Title := FindAttribute('TITLE');
    Style := FindAttribute('STYLE');
  end;
end;

{ TIpHtmlNodeINS }

procedure TIpHtmlNodeINS.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontStyle := Props.FontStyle + [fsUnderline];
end;

{ TIpHtmlNodeDEL }

procedure TIpHtmlNodeDEL.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.FontStyle := Props.FontStyle + [fsStrikeOut];
end;

{ TIpHtmlNodeTHEAD }

constructor TIpHtmlNodeTHEAD.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  FVAlign := hva3Middle;
end;

{ TIpHtmlNodeTBODY }

constructor TIpHtmlNodeTBODY.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  FVAlign := hva3Middle;
end;

{ TIpHtmlNodeSTYLE }

{!!.10 new}
function TIpHtmlNodeSTYLE.ElementQueueIsEmpty: Boolean;
begin
  Result := True;
end;

procedure TIpHtmlNodeSTYLE.EnqueueElement(const Entry: PIpHtmlElement);
begin
end;

{ TIpHtmlNodeIFRAME }

procedure TIpHtmlNodeIFRAME.CreateControl(Parent: TWinControl);
begin
  Owner.ControlCreate(Self);
  Owner.CreateIFrame(Parent, Self, FControl);
end;

procedure TIpHtmlNodeIFRAME.AddValues(NameList, ValueList: TStringList);
begin
end;

procedure TIpHtmlNodeIFRAME.Reset;
begin
end;

function TIpHtmlNodeIFRAME.Successful: Boolean;
begin
  Result := False;
end;

{!!.10 new}
destructor TIpHtmlNodeIFRAME.Destroy;
begin
  inherited;
  FHeight.Free;
  FWidth.Free;
end;

procedure TIpHtmlNodeIFRAME.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

procedure TIpHtmlNodeIFRAME.SetAlign(const Value: TIpHtmlAlign);
begin
  if Value <> FAlign then begin
    FAlign := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetFrameBorder(const Value: Integer);
begin
  if Value <> FFrameBorder then begin
    FFrameBorder := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetMarginHeight(const Value: Integer);
begin
  if Value <> FMarginHeight then begin
    FMarginHeight := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetMarginWidth(const Value: Integer);
begin
  if Value <> FMarginWidth then begin
    FMarginWidth := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetScrolling(
  const Value: TIpHtmlFrameScrolling);
begin
  if Value <> FScrolling then begin
    FScrolling := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeBUTTON }

procedure TIpHtmlNodeBUTTON.AddValues(NameList, ValueList : TStringList);
begin
end;

constructor TIpHtmlNodeBUTTON.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Owner.ControlList.Add(Self);
end;

procedure TIpHtmlNodeBUTTON.CreateControl(Parent: TWinControl);
begin
  Owner.ControlCreate(Self);
  FControl := TButton.Create(Parent);
  FControl.Visible := False;
  FControl.Parent := Parent;
  with TButton(FControl) do begin
    Enabled := not Self.Disabled;
    Caption := Value;
    case ButtonType of
    hbtSubmit :
      begin
        OnClick := SubmitClick;
        if Caption = '' then
          Caption := SHtmlDefSubmitCaption;
      end;
    hbtReset :
      begin
        OnClick := ResetClick;
        if Caption = '' then
          Caption := SHtmlDefResetCaption;
      end;
    hbtButton :
      begin
        OnClick := ButtonClick;
      end;
    end;
    Width := TFriendPanel(Parent).Canvas.TextWidth(Caption) + 40;
    Height := TFriendPanel(Parent).Canvas.TextHeight(Caption) + 10;
  end;
end;

destructor TIpHtmlNodeBUTTON.Destroy;
begin
  Owner.ControlList.Remove(Self);
  inherited;
end;

procedure TIpHtmlNodeBUTTON.Reset;
begin
end;

procedure TIpHtmlNodeBUTTON.ResetClick(Sender: TObject);
begin
  ResetRequest;
end;

procedure TIpHtmlNodeBUTTON.SubmitClick(Sender: TObject);
begin
  SubmitRequest;
end;

procedure TIpHtmlNodeBUTTON.ButtonClick(Sender: TObject);
begin
  Owner.ControlClick(Self);
end;

function TIpHtmlNodeBUTTON.Successful: Boolean;
begin
  Result := False;
end;

{ TIpHtmlNodeCOL }

{!!.10 new}
destructor TIpHtmlNodeCOL.Destroy;
begin
  inherited;
  FWidth.Free;
end;

{ TIpHtmlNodeCOLGROUP }

{!!.10 new}
destructor TIpHtmlNodeCOLGROUP.Destroy;
begin
  inherited;
  FWidth.Free;
end;

{ TIpHtmlNodeLABEL }

constructor TIpHtmlNodeLABEL.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Owner.ControlList.Add(Self);
end;

destructor TIpHtmlNodeLABEL.Destroy;
begin
  Owner.ControlList.Remove(Self);
  inherited;
end;

{ TIpHtmlNodeNOBR }

procedure TIpHtmlNodeNOBR.ApplyProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.NoBreak := True;
end;

{ TIpHtmlProps }

function TIpHtmlProps.AIsEqualTo(Compare: TIpHtmlProps): Boolean;
begin
  Result :=
    (PropA = Compare.PropA);
end;

procedure TIpHtmlProps.Assign(Source: TIpHtmlProps);
begin
  if PropA <> Source.PropA then begin
    PropA.DecUse;
    PropA := Source.PropA;
    PropA.IncUse;
  end;
  if PropB <> Source.PropB then begin
    PropB.DecUse;
    PropB := Source.PropB;
    PropB.IncUse;
  end;
end;

function TIpHtmlProps.BIsEqualTo(Compare: TIpHtmlProps): Boolean;
begin
  Result :=
    (PropB = Compare.PropB);
end;

constructor TIpHtmlProps.Create(Owner: TIpHtml);
begin
  FOwner := Owner;
  PropA := Owner.DummyA;
  PropA.IncUse;
  PropB := Owner.DummyB;
  PropB.IncUse;
end;

destructor TIpHtmlProps.Destroy;
begin
  PropA.DecUse;
  PropB.DecUse;
  inherited;
end;

function TIpHtmlProps.GetAlignment: TIpHtmlAlign;
begin
  Result := PropB.Alignment;
end;

function TIpHtmlProps.GetALinkColor: TColor;
begin
  Result := PropB.ALinkColor;
end;

function TIpHtmlProps.GetBaseFontSize: integer;
begin
  Result := PropA.BaseFontSize;
end;

function TIpHtmlProps.GetBgColor: TColorRef;
begin
  Result := PropB.BgColor;
end;

function TIpHtmlProps.GetFontBaseline: integer;
begin
  Result := PropB.FontBaseline;
end;

function TIpHtmlProps.GetFontColor: TColor;
begin
  Result := PropB.FontColor;
end;

function TIpHtmlProps.GetFontName: string;
begin
  Result := PropA.FontName;
end;

function TIpHtmlProps.GetFontSize: integer;
begin
  Result := PropA.FontSize;
end;

function TIpHtmlProps.GetFontStyle: TFontStyles;
begin
  Result := PropA.FontStyle;
end;

function TIpHtmlProps.GetLinkColor: TColor;
begin
  Result := PropB.LinkColor;
end;

function TIpHtmlProps.GetNoBreak: Boolean;
begin
  Result := PropB.NoBreak;
end;

function TIpHtmlProps.GetPreformatted: Boolean;
begin
  Result := PropB.Preformatted;
end;

function TIpHtmlProps.GetVAlignment: TIpHtmlVAlign3;
begin
  Result := PropB.VAlignment;
end;

function TIpHtmlProps.GetVLinkColor: TColor;
begin
  Result := PropB.VLinkColor;
end;

function TIpHtmlProps.IsEqualTo(Compare: TIpHtmlProps): Boolean;
begin
  Result :=
    (PropA = Compare.PropA)
    and (PropB = Compare.PropB);
end;

function TIpHtml.FindPropA(
  const pFontName : string;
  const pFontSize : Integer;
  const pFontStyle : TFontStyles;
  const pBaseFontSize : Integer): TIpHtmlPropA;
var
  i: Integer;
begin
  for i := 0 to pred(PropACache.Count) do begin
    Result := TIpHtmlPropA(PropACache[i]);
    with Result do begin
      if FontStyle <> pFontStyle then continue;
      if FontSize <> pFontSize then continue;
      if FontName <> pFontName then continue;
      if BaseFontSize <> pBaseFontSize then continue;
      exit;
    end;
  end;
  Result := nil;
end;

function TIpHtml.FindPropB(
  const pFontBaseline : integer;
  const pFontColor : TColor;
  const pAlignment : TIpHtmlAlign;
  const pVAlignment : TIpHtmlVAlign3;
  const pLinkColor : TColor;
  const pVLinkColor : TColor;
  const pALinkColor : TColor;
  const pBgColor : TColorRef;
  const pPreformatted : Boolean;
  const pNoBreak : Boolean
  ): TIpHtmlPropB;
var
  i: Integer;
begin
  for i := 0 to pred(PropBCache.Count) do begin
    Result := TIpHtmlPropB(PropBCache[i]);
    with Result do begin
      if VAlignment <> pVAlignment then continue;
      if FontColor <> pFontColor then continue;
      if Alignment <> pAlignment then continue;
      if LinkColor <> pLinkColor then continue;
      if VLinkColor <> pVLinkColor then continue;
      if ALinkColor <> pALinkColor then continue;
      if BgColor <> pBgColor then continue;
      if Preformatted <> pPreformatted then continue;
      if NoBreak <> pNoBreak then continue;
      if FontBaseline <> pFontBaseline then continue;
      exit;
    end;
  end;
  Result := nil;
end;

procedure TIpHtmlProps.SetAlignment(const Value: TIpHtmlAlign);
var
  NewPropB : TIpHtmlPropB;
begin
  if (Value <> haDefault) and (Value <> Alignment) then begin
    NewPropB := FOwner.FindPropB(FontBaseline, FontColor, Value,
      VAlignment, LinkColor, VLinkColor, ALinkColor,
      BgColor, Preformatted, NoBreak);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FAlignment := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetALinkColor(const Value: TColor);
var
  NewPropB : TIpHtmlPropB;
begin
  if Value <> ALinkColor then begin
    NewPropB := FOwner.FindPropB(FontBaseline, FontColor, Alignment,
      VAlignment, LinkColor, VLinkColor, Value,
      BgColor, Preformatted, NoBreak);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FAlinkColor := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetBaseFontSize(const Value: integer);
var
  NewPropA : TIpHtmlPropA;
begin
  if Value <> BaseFontSize then begin
    NewPropA := FOwner.FindPropA(FontName, FontSize, FontStyle, Value);
    if NewPropA = nil then begin
      NewPropA := TIpHtmlPropA.CreateCopy(PropA);
      NewPropA.FBaseFontSize := Value;
      FOwner.PropACache.Add(NewPropA);
    end;
    NewPropA.IncUse;
    PropA.DecUse;
    PropA := NewPropA;
  end;
end;

procedure TIpHtmlProps.SetBgColor(const Value: TColorRef);
var
  NewPropB : TIpHtmlPropB;
begin
  if Value <> BgColor then begin
    NewPropB := FOwner.FindPropB(FontBaseline, FontColor, Alignment,
      VAlignment, LinkColor, VLinkColor, ALinkColor,
      Value, Preformatted, NoBreak);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FBgColor := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetFontBaseline(const Value: integer);
var
  NewPropB : TIpHtmlPropB;
begin
  if Value <> FontBaseline then begin
    NewPropB := FOwner.FindPropB(Value, FontColor, Alignment,
      VAlignment, LinkColor, VLinkColor, ALinkColor,
      bgColor, Preformatted, NoBreak);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FFontBaseline := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetFontColor(const Value: TColor);
var
  NewPropB : TIpHtmlPropB;
begin
  if Value <> FontColor then begin
    NewPropB := FOwner.FindPropB(FontBaseline, Value, Alignment,
      VAlignment, LinkColor, VLinkColor, ALinkColor,
      bgColor, Preformatted, NoBreak);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FFontColor := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetFontName(const Value: string);
var
  NewPropA : TIpHtmlPropA;
begin
  if Value <> FontName then begin
    NewPropA := FOwner.FindPropA(Value, FontSize, FontStyle, BaseFontSize);
    if NewPropA = nil then begin
      NewPropA := TIpHtmlPropA.CreateCopy(PropA);
      NewPropA.FFontName := Value;
      FOwner.PropACache.Add(NewPropA);
    end;
    NewPropA.IncUse;
    PropA.DecUse;
    PropA := NewPropA;
  end;
end;

procedure TIpHtmlProps.SetFontSize(const Value: integer);
var
  NewPropA : TIpHtmlPropA;
begin
  if Value <> FontSize then begin
    NewPropA := FOWner.FindPropA(FontName, Value, FontStyle, BaseFontSize);
    if NewPropA = nil then begin
      NewPropA := TIpHtmlPropA.CreateCopy(PropA);
      NewPropA.FFontSize := Value;
      FOwner.PropACache.Add(NewPropA);
    end;
    NewPropA.IncUse;
    PropA.DecUse;
    PropA := NewPropA;
  end;
end;

procedure TIpHtmlProps.SetFontStyle(const Value: TFontStyles);
var
  NewPropA : TIpHtmlPropA;
begin
  if Value <> FontStyle then begin
    NewPropA := FOwner.FindPropA(FontName, FontSize, Value, BaseFontSize);
    if NewPropA = nil then begin
      NewPropA := TIpHtmlPropA.CreateCopy(PropA);
      NewPropA.FFontStyle := Value;
      FOwner.PropACache.Add(NewPropA);
    end;
    NewPropA.IncUse;
    PropA.DecUse;
    PropA := NewPropA;
  end;
end;

procedure TIpHtmlProps.SetLinkColor(const Value: TColor);
var
  NewPropB : TIpHtmlPropB;
begin
  if Value <> LinkColor then begin
    NewPropB := FOwner.FindPropB(FontBaseline, FontColor, Alignment,
      VAlignment, Value, VLinkColor, ALinkColor,
      bgColor, Preformatted, NoBreak);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FLinkColor := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetNoBreak(const Value: Boolean);
var
  NewPropB : TIpHtmlPropB;
begin
  if Value <> NoBreak then begin
    NewPropB := FOwner.FindPropB(FontBaseline, FontColor, Alignment,
      VAlignment, LinkColor, VLinkColor, ALinkColor,
      bgColor, Preformatted, Value);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FNoBreak := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetPreformatted(const Value: Boolean);
var
  NewPropB : TIpHtmlPropB;
begin
  if Value <> Preformatted then begin
    NewPropB := FOwner.FindPropB(FontBaseline, FontColor, Alignment,
      VAlignment, LinkColor, VLinkColor, ALinkColor,
      bgColor, Value, NoBreak);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FPreformatted := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetVAlignment(const Value: TIpHtmlVAlign3);
var
  NewPropB : TIpHtmlPropB;
begin
  if Value <> VAlignment then begin
    NewPropB := FOwner.FindPropB(FontBaseline, FontColor, Alignment,
      Value, LinkColor, VLinkColor, ALinkColor,
      bgColor, Preformatted, NoBreak);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FVAlignment := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

procedure TIpHtmlProps.SetVLinkColor(const Value: TColor);
var
  NewPropB : TIpHtmlPropB;
begin
  if Value <> VLinkColor then begin
    NewPropB := FOwner.FindPropB(FontBaseline, FontColor, Alignment,
      VAlignment, LinkColor, Value, ALinkColor,
      bgColor, Preformatted, NoBreak);
    if NewPropB = nil then begin
      NewPropB := TIpHtmlPropB.CreateCopy(FOwner, PropB);
      NewPropB.FVLinkColor := Value;
      FOwner.PropBCache.Add(NewPropB);
    end;
    NewPropB.IncUse;
    PropB.DecUse;
    PropB := NewPropB;
  end;
end;

{ TIpHtmlPropA }

procedure TIpHtmlPropA.Assign(const Source: TIpHtmlPropA);
begin
  if Source <> nil then begin
    FontName := Source.FontName;
    FontSize := Source.FontSize;
    FontStyle := Source.FontStyle;
    BaseFontSize := Source.BaseFontSize;
  end;
end;

constructor TIpHtmlPropA.CreateCopy(Source: TIpHtmlPropA);
begin
  inherited Create;
  Assign(Source);
end;

procedure TIpHtmlPropA.DecUse;
begin
  dec(FUseCount);
end;

procedure TIpHtmlPropA.IncUse;
begin
  inc(FUseCount);
end;

procedure TIpHtmlPropA.SetBaseFontSize(const Value: integer);
begin
  if Value <> FBaseFontSize then begin
    FBaseFontSize := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetFontName(const Value: string);
begin
  if Value <> FFontName then begin
    FFontName := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetFontSize(const Value: integer);
begin
  if Value <> FFontSize then begin
    FFontSize := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetFontStyle(const Value: TFontStyles);
begin
  if Value <> FFontStyle then begin
    FFontStyle := Value;
    FSizeOfSpaceKnown := False;
  end;
end;

procedure TIpHtmlPropA.SetKnownSizeOfSpace(const Size: TSize);
begin
  if Size.cx = 0 then
    raise EIpHtmlException.Create(SHtmlInternal);              {!!.02}
  FKnownSizeOfSpace := Size;
  FSizeOfSpaceKnown := True;
end;

{ TIpHtmlPropB }

procedure TIpHtmlPropB.Assign(const Source: TIpHtmlPropB);
begin
  if Source <> nil then begin
    FontBaseline := Source.Fontbaseline;
    FontColor := Source.FontColor;
    Alignment := Source.Alignment;
    VAlignment := Source.VAlignment;
    LinkColor := Source.LinkColor;
    ALinkColor := Source.ALinkColor;
    VLinkColor := Source.VLinkColor;
    Preformatted := Source.Preformatted;
    BgColor := Source.BgColor;
    NoBreak := Source.NoBreak;
  end;
end;

constructor TIpHtmlPropB.Create(Owner: TIpHtml);
begin
  inherited Create;
  FOwner := Owner;
end;

constructor TIpHtmlPropB.CreateCopy(Owner: TIpHtml; Source: TIpHtmlPropB);
begin
  inherited Create;
  FOwner := Owner;
  Assign(Source);
end;

procedure TIpHtmlPropB.DecUse;
var
  i, c: Integer;
begin
  dec(FUseCount);
  if UseCount = 0 then begin
    for i := pred(FOwner.PropBCache.Count) downto 0 do
      if FOwner.PropBCache[i] = Self then begin
        c := FOwner.PropBCache.Count;
        if (c > 1)
        and (i < c - 1) then
          FOwner.PropBCache[i] := FOwner.PropBCache[c - 1];
        FOwner.PropBCache.Delete(c - 1);
        Free;
        exit;
      end;
    raise EIpHtmlException.Create(SHtmlInternal);              {!!.02}
  end else
    if UseCount < 0 then
      raise EIpHtmlException.Create(SHtmlInternal);            {!!.02}
end;

procedure TIpHtmlPropB.IncUse;
begin
  inc(FUseCount);
end;

{ TIpHtmlNodeTableHeaderOrCell }

procedure TIpHtmlNodeTableHeaderOrCell.CalcMinMaxWidth(
  const RenderProps: TIpHtmlProps; var Min, Max: Integer);
begin
  Props.Assign(RenderProps);
  Props.Alignment := Align;
  if Self is TIpHtmlNodeTH then
    Props.FontStyle := Props.FontStyle + [fsBold];
  Props.VAlignment := VAlign;
  if BgColor <> -1 then
    Props.BgColor := BgColor;
  if NoWrap then
    Props.NoBreak := True;
  inherited CalcMinMaxWidth(Props, Min, Max);
  if NoWrap then                                                       {!!.10}
    Min := Max;                                                        {!!.10}
end;

procedure TIpHtmlNodeTableHeaderOrCell.Render(
  const RenderProps: TIpHtmlProps);
var
  R : TRect;
begin
  Props.Assign(RenderProps);
  if Align <> haDefault then
    Props.Alignment := Align
  else
    if Self is TIpHtmlNodeTH then
      Props.Alignment := haCenter
    else
      Props.Alignment := haLeft;
  if Self is TIpHtmlNodeTH then
    Props.FontStyle := Props.FontStyle + [fsBold];
  Props.VAlignment := VAlign;
  if NoWrap then
    Props.NoBreak := True;
  {$IFDEF IP_LAZARUS}
  //DebugBox(Owner.Target, PadRect, clYellow, True);
  {$ENDIF}
  if PageRectToScreen(PadRect, R) then begin
    if (BgColor <> -1) then begin
      Props.BgColor := BgColor;
      Owner.Target.Brush.Color := BGColor;
      Owner.Target.FillRect(R);
    end;
  end;
  inherited Render(Props);
end;

constructor TIpHtmlNodeTableHeaderOrCell.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FRowSpan := 1;
  FColSpan := 1;
  FAlign := haDefault;
  FVAlign := hva3Middle;
  {FHeight := -1;}                                                     {!!.10}
  BgColor := -1;
end;

procedure TIpHtmlNodeTableHeaderOrCell.Layout(
  const RenderProps: TIpHtmlProps; const TargetRect: TRect);
begin
  Props.Assign(RenderProps);
  if Align <> haDefault then
    Props.Alignment := Align
  else
    if Self is TIpHtmlNodeTH then
      Props.Alignment := haCenter;
  if Self is TIpHtmlNodeTH then
    Props.FontStyle := Props.FontStyle + [fsBold];
  if NoWrap then
    Props.NoBreak := True;
  case VAlign of
  hva3Default :;
  else
    Props.VAlignment := VAlign;
  end;
  if BgColor <> -1 then
    Props.BgColor := BgColor;
  inherited Layout(Props, TargetRect);
end;

destructor TIpHtmlNodeTableHeaderOrCell.Destroy;
begin
  inherited;
  FWidth.Free;                                                         {!!.10}
  FHeight.Free;                                                        {!!.10}
end;

{!!.10 new}
procedure TIpHtmlNodeTableHeaderOrCell.DimChanged(Sender: TObject);
begin
  InvalidateSize;
end;

{ TIpHtmlNodeInline }

procedure TIpHtmlNodeInline.Invalidate;
begin
  FParentNode.Invalidate;
end;

procedure TIpHtmlNodeInline.EnqueueElement(
  const Entry: PIpHtmlElement);
begin
  FParentNode.EnqueueElement(Entry);
end;

{!!.10 new}
function TIpHtmlNodeInline.ElementQueueIsEmpty: Boolean;
begin
  Result := FParentNode.ElementQueueIsEmpty;
end;

{ TIpHtmlNodeAlignInline }

constructor TIpHtmlNodeAlignInline.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  Props := TIpHtmlProps.Create(FOwner);
  Element := Owner.NewElement(etObject, Self);
  Element.Props := Props;
end;

destructor TIpHtmlNodeAlignInline.Destroy;
begin
  Props.Free;
  inherited;
end;

procedure TIpHtmlNodeAlignInline.Enqueue;
begin
  EnqueueElement(Element);
end;

procedure TIpHtmlNodeAlignInline.SetAlignment(
  const Value: TIpHtmlImageAlign);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TIpHtmlNodeAlignInline.SetRect(TargetRect: TRect);
begin
end;

{ TIpHtmlNodeControl }

procedure TIpHtmlNodeControl.CalcMinMaxWidth(var Min, Max: Integer);
begin
  if FControl <> nil then
    Min := FControl.Width
  else
    Min := 0;
  Max := Min;
end;

constructor TIpHtmlNodeControl.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Owner.ControlList.Add(Self);
  Align := hiaBottom;
end;

destructor TIpHtmlNodeControl.Destroy;
begin
  Owner.ControlList.Remove(Self);
  inherited;
end;

procedure TIpHtmlNodeControl.Draw;
var
  R : TRect;
  TopLeft : TPoint;
  Dim : TSize;
begin
  if FControl <> nil then begin
    TopLeft := Element.WordRect2.TopLeft;
    R.TopLeft := TopLeft;
    Dim := GetDim(0);
    R.Right := TopLeft.x + Dim.cx;
    R.Bottom := TopLeft.y + Dim.cy;
    if PageRectToScreen(R, R) then begin
      FControl.Left := R.Left;
      FCOntrol.Top := R.Top;
      FControl.Visible := True;
      Shown := not ScaleBitmaps{True}; {Keep controls hidden during printing} {!!.10}
    end else
      FControl.Visible := False;
  end;
end;

procedure TIpHtmlNodeControl.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
end;

function TIpHtmlNodeControl.GetDim(ParentWidth: Integer): TSize;
begin
  if FControl <> nil then
    Result := SizeRec(FControl.Width, FControl.Height)
  else
    Result := SizeRec(0, 0);
end;

procedure TIpHtmlNodeControl.HideUnmarkedControl;
begin
  if not Shown and (FControl <> nil) then
    FControl.Visible := False;
end;

procedure TIpHtmlNodeControl.UnmarkControl;
begin
  Shown := False;
end;

{ TIpHtmlNodeNv }

procedure TIpHtmlNodeNv.Invalidate;
begin
end;

procedure TIpHtmlNodeNv.InvalidateSize;
begin
end;

procedure TIpHtmlNodeNv.EnqueueElement(
  const Entry: PIpHtmlElement);
begin
end;

procedure TIpHtmlNodeNv.ReportDrawRects(M: TRectMethod);
begin
end;

procedure TIpHtmlNodeNv.SetProps(const RenderProps: TIpHtmlProps);
begin
end;

procedure TIpHtmlNodeNv.Enqueue;
begin
end;

{!!.10 new}
function TIpHtmlNodeNv.ElementQueueIsEmpty: Boolean;
begin
  Result := True;
end;

{ TIpHtmlNodeFRAME }

procedure TIpHtmlNodeFRAME.SetFrameBorder(const Value: Integer);
begin
  if Value <> FFrameBorder then begin
    FFrameBorder := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFRAME.SetMarginHeight(const Value: Integer);
begin
  if Value <> FMarginHeight then begin
    FMarginHeight := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFRAME.SetMarginWidth(const Value: Integer);
begin
  if Value <> FMarginWidth then begin
    FMarginWidth := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeFRAME.SetScrolling(
  const Value: TIpHtmlFrameScrolling);
begin
  if Value <> FScrolling then begin
    FScrolling := Value;
    InvalidateSize;
  end;
end;

{ TIpHtmlNodeFRAMESET }

{!!.10 new}
destructor TIpHtmlNodeFRAMESET.Destroy;
begin
  inherited;
  FCols.Free;
  FRows.Free;
end;

{ TIpHtmlNodeGenInline }

constructor TIpHtmlNodeGenInline.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Props := TIpHtmlProps.Create(Owner);
end;

destructor TIpHtmlNodeGenInline.Destroy;
begin
  Props.Free;
  inherited;
end;

procedure TIpHtmlNodeGenInline.SetProps(const RenderProps: TIpHtmlProps);
begin
  ApplyProps(RenderProps);
  inherited SetProps(Props);
end;

{ TIpHtmlInternalPanel }

constructor TIpHtmlInternalPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse];
  DragMode := dmManual;
  HScroll := TIpHtmlScrollBar.Create(Self, sbHorizontal);
  HScroll.Tracking := True;
  VScroll := TIpHtmlScrollBar.Create(Self, sbVertical);
  VScroll.Tracking := True;
  HintWindow := THintWindow.Create(Self);
  HintWindow.Color := Application.HintColor;
end;

destructor TIpHtmlInternalPanel.Destroy;
begin
  HScroll.Free;
  VScroll.Free;
  HintWindow.Free;
  inherited;
end;

procedure TIpHtmlInternalPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or WS_HSCROLL or WS_VSCROLL;
end;

procedure TIpHtmlInternalPanel.DoHotChange;
begin
  if assigned(FOnHotChange) then
    FOnHotChange(Self);
end;

procedure TIpHtmlInternalPanel.DoCurElementChange;
begin
  if assigned(FOnCurElementChange) then
    FOnCurElementChange(Self);
end;

procedure TIpHtmlInternalPanel.DoHotInvoke;
begin
  if assigned(FOnHotClick) then
    FOnHotClick(Hyper);
end;

procedure TIpHtmlInternalPanel.DoClick;
begin
  if assigned(FOnClick) then
    FOnClick(Hyper);
end;

procedure TIpHtmlInternalPanel.ShowHintNow(const NewHint: string);     {!!.12}
var
  Tw,Th : Integer;
  Sc : TPoint;
begin
  if HtmlPanel.ShowHints and (NewHint <> CurHint) then begin
    {$IFDEF IP_LAZARUS}
    if (NewHint<>'') and not HintWindow.Visible then begin
      Tw := HintWindow.Canvas.TextWidth(NewHint);
      Th := HintWindow.Canvas.TextHeight(NewHint);
      Sc := ClientToScreen(Point(HintX,HintY));
      HintWindow.ActivateHint(Rect(Sc.X - Tw div 2 - 6,
                                   Sc.Y + 16 - 6,
                                   Sc.X + Tw div 2 + 6,
                                   Sc.Y + Th + 16 + 6),
                              NewHint);
    end;
    {$ELSE}
    if (NewHint <> '') and not IsWindowVisible(HintWindow.Handle) then begin
      Tw := HintWindow.Canvas.TextWidth(NewHint);
      Th := HintWindow.Canvas.TextHeight(NewHint);
      Sc := ClientToScreen(Point(HintX,HintY));
      HintWindow.ActivateHint(Rect(Sc.X - Tw div 2 - 4,
                                   Sc.Y + 16,
                                   Sc.X + Tw div 2 + 4,
                                   Sc.Y + Th + 16),
                              NewHint);
    end;
    {$ENDIF}
    CurHint := NewHint;
    HintShownHere := True;
  end;
end;

procedure TIpHtmlInternalPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldHot : TIpHtmlNode;
  OldCurElement : PIpHtmlElement;
begin
  if MouseIsDown and HaveSelection then begin
    SelEnd := Point(X + ViewLeft, Y + ViewTop);
    SetSelection;
    ScrollPtInView(Point(X + ViewLeft, Y + ViewTop));
  end;
  if Hyper <> nil then begin
    OldHot := Hyper.HotNode;
    OldCurElement := Hyper.CurElement;
    Hyper.MouseMove(Point(X + ViewLeft, Y + ViewTop));
    if (Hyper.HotNode <> OldHot) or (Hyper.HotPoint.x >= 0) then
      DoHotChange;
    if Hyper.HotNode <> nil then begin
      if Hyper.CurElement <> nil then begin
        Hyper.CurElement := nil;
        if OldCurElement <> Hyper.CurElement then
          DoCurElementChange;
      end;
    end else begin
      if HtmlPanel.AllowTextSelect then begin
        if Hyper.CurElement <> nil then begin
          if Hyper.CurElement.ElementType = etWord then
            Cursor := crIBeam
          else
            Cursor := crDefault;
        end else
          Cursor := crDefault;
      end;
      if OldCurElement <> Hyper.CurElement then
        DoCurElementChange;
    end;
  end;
  if (Hyper <> nil) and (Hyper.HotNode <> nil) then
    Hint := Hyper.HotNode.GetHint
  else
  if (Hyper <> nil) and (Hyper.CurElement <> nil)
  and (Hyper.CurElement.ElementType = etObject)
  and (Hyper.CurElement.Owner <> nil) then
    Hint := Hyper.CurElement.Owner.GetHint
  else
    Hint := '';
  {$IFNDEF IP_LAZARUS}
  if NewSelection then begin
    ClearSelection;
    SelStart := Point(X + ViewLeft, Y + ViewTop);
    NewSelection := False;
    HaveSelection := True;
  end;
  {$ENDIF}
  inherited;
  if (Hint <> CurHint) and ((abs(HintX - X) > 4) or (abs(HintY - Y) > 4)) then begin
    {$IFDEF IP_LAZARUS}
    if HintWindow.Visible then
    {$ELSE}
    if IsWindowVisible(HintWindow.Handle) then
    {$ENDIF}
      HideHint;
    HintShownHere := False;
  end;
  HintX := X;
  HintY := Y;
  if not HintShownHere then
    ShowHintNow(Hint);
end;

procedure TIpHtmlInternalPanel.HideHint;
begin
  {$IFDEF IP_LAZARUS}
  HintWindow.Visible := False;
  {$ELSE}
  HintWindow.ReleaseHandle;
  {$ENDIF}
end;

procedure TIpHtmlInternalPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  MouseDownX := X;
  MouseDownY := Y;
  MouseIsDown := True;
  {$IFDEF IP_LAZARUS}
  if (Button=mbLeft) and HtmlPanel.AllowTextSelect then begin
    ClearSelection;
    SelStart := Point(X + ViewLeft, Y + ViewTop);
    NewSelection := False;
    HaveSelection := True;
  end;
  {$ELSE}
  NewSelection := HtmlPanel.AllowTextSelect
    {TIpHtmlPanel(Parent).AllowTextSelect} and (Button = mbLeft);
  {$ENDIF}
  inherited;
end;

procedure TIpHtmlInternalPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  MouseIsDown := False;
  if (abs(MouseDownX - X) < 4)
  and (abs(MouseDownY - Y) < 4) then
    if (Button = mbLeft) and (Hyper.HotNode <> nil) then
      {$IFDEF IP_LAZARUS}
      // to avoid references to invalid objects do it asynchronously
      Application.QueueAsyncCall(AsyncHotInvoke, 0)
      {$ELSE}
      DoHotInvoke
      {$ENDIF}
    else
      DoClick;
end;

procedure TIpHtmlInternalPanel.Paint;
var
  CR : TRect;
begin
  CR := GetClientRect;
  if not ScaleBitmaps {printing}                                       {!!.10}
  and (Hyper <> nil) then
    Hyper.Render(Canvas,
      Rect(
        ViewLeft, ViewTop,
          ViewLeft + (CR.Right - CR.Left),
          ViewTop + (CR.Bottom - CR.Top)),
          True,
          Point(0, 0))                                                 {!!.10}
  else
    Canvas.FillRect(CR);
  {$IFDEF IP_LAZARUS}
  //DebugBox(CR, clYellow);
  //Debugbox(Canvas.ClipRect,clLime, true);
  {$ENDIF}
end;

{!!.10 new}
procedure TIpHtmlInternalPanel.BeginPrint;
var
  LogPixX, LMarginPix, RMarginPix,
  LogPixY, TMarginPix, BMarginPix,
  H: Integer;
begin
  if InPrint = 0 then begin
    SetRectEmpty(PrintPageRect);
    if Hyper.TitleNode <> nil then
      Printer.Title := Hyper.TitleNode.Title
    else
      Printer.Title := 'HTML Document';
    Printer.BeginDoc;
    Printed := False;
    ScaleBitmaps := True;
    GetRelativeAspect(Printer.Canvas.Handle);
    BWPrinter := GetDeviceCaps(Printer.Canvas.Handle, PLANES) = 1;
    LogPixX := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX);
    LMarginPix := round(HtmlPanel.PrintSettings.MarginLeft * LogPixX);
    RMarginPix := round(HtmlPanel.PrintSettings.MarginRight * LogPixX);
    PrintWidth := Printer.PageWidth - LMarginPix - RMarginPix;
    LogPixY := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY);
    TMarginPix := round(HtmlPanel.PrintSettings.MarginTop * LogPixY);
    BMarginPix := round(HtmlPanel.PrintSettings.MarginBottom * LogPixY);
    PrintHeight := Printer.PageHeight - TMarginPix - BMarginPix;
    PrintTopLeft := Point(LMarginPix, TMarginPix);
    {PrintBottomRight := Point(
      Printer.PageWidth - RMarginPix,
      Printer.PageHeight - BMarginPix);}                               {!!.12}
    PrintPageRect := Hyper.GetPageRect(Printer.Canvas,
      PrintWidth, PrintHeight);
    H := PrintPageRect.Bottom - PrintPageRect.Top;
    PageCount := H div PrintHeight;
    if H mod PrintHeight <> 0 then
      inc(PageCount);
  end;
  inc(InPrint);
end;

{!!.10 new}
procedure TIpHtmlInternalPanel.EndPrint;
begin
  dec(InPrint);
  if InPrint = 0 then begin
    if Printed then
      Printer.EndDoc
    else
      Printer.Abort;
    ScaleBitmaps := False;
    InvalidateSize;
  end;
end;

procedure TIpHtmlInternalPanel.PrintPages(FromPage, ToPage: Integer);
var
  CR : TRect;
var
  i : Integer;
begin
  {CR := Rect(0, 0, Printer.PageWidth, 0);}
  if (Hyper <> nil) then begin
    {Printer.BeginDoc;}
    BeginPrint;                                                        {!!.10}
    try
      (*
      ScaleBitmaps := True;                                            {!!.02}
      GetRelativeAspect(Printer.Canvas.Handle);                        {!!.02}
      PrintPageRect := Hyper.GetPageRect(Printer.Canvas,
        Printer.PageWidth, Printer.PageHeight);
      *)
      CR := Rect(0, 0, PrintWidth, 0);                                 {!!.10}
      for i := FromPage to ToPage do begin
        CR.Top := (i - 1) * PrintHeight;                               {!!.10}
        CR.Bottom := Cr.Top + PrintHeight;                             {!!.10}
        Hyper.Render(Printer.Canvas, CR, False, PrintTopLeft);         {!!.10}
        if i < ToPage then
          Printer.NewPage;
        Printed := True;                                               {!!.10}
      end;
    finally
      {ScaleBitmaps := False;}                                         {!!.10}
      {Printer.EndDoc;}                                                {!!.10}
      {InvalidateSize;}                                                {!!.10}
      EndPrint;                                                        {!!.10}
    end;
  end;
end;

{!!.10 new}
procedure TIpHtmlInternalPanel.PrintPreview;
begin
  if (Hyper <> nil) then begin
    BeginPrint;
    try

      with TIpHTMLPreview.Create(Application) do
        try
          lblMaxPage.Caption := IntToStr(PageCount);
          FCurPage := 1;
          HTML := Hyper;
          ScaleFonts := True;
          try
            OwnerPanel := Self;
            ShowModal;
          finally
            ScaleFonts := False;
          end;
        finally
          Free;
        end;

    finally
      EndPrint;
    end;
  end;
end;

procedure TIpHtmlInternalPanel.EraseBackground(DC: HDC);
begin
  //
end;

function TIpHtmlInternalPanel.GetPrintPageCount: Integer;
{var
  H : Integer;}                                                        {!!.10}
begin
  BeginPrint;                                                          {!!.10}
  try                                                                  {!!.10}
    Result := PageCount;                                               {!!.10}
  finally                                                              {!!.10}
    EndPrint;                                                          {!!.10}
  end;                                                                 {!!.10}
  {!!.10
  SetRectEmpty(PrintPageRect);
  if Hyper <> nil then begin
    PrintPageRect := Hyper.GetPageRect(Printer.Canvas,
      Printer.PageWidth, Printer.PageHeight);
  end;
  H := PrintPageRect.Bottom - PrintPageRect.Top;
  Result := H div Printer.PageHeight;
  if H mod Printer.PageHeight <> 0 then
    inc(Result);
  }
end;

procedure TIpHtmlInternalPanel.InvalidateSize;
begin
  if Hyper <> nil then
    PageRect := Hyper.GetPageRect(Canvas, ClientWidth, 0);
  Invalidate;
end;

procedure TIpHtmlInternalPanel.Resize;
begin
  inherited;
  InvalidateSize;
end;

function TIpHtmlInternalPanel.PagePtToScreen(const Pt : TPoint): TPoint;
{-convert coordinates of point passed in to screen coordinates}
begin
  Result := Pt;
  dec(Result.x, ViewLeft);
  dec(Result.y, ViewTop);
end;

procedure TIpHtmlInternalPanel.ScrollInViewRaw(R : TRect);
begin
  R.TopLeft := PagePtToScreen(R.TopLeft);
  R.BottomRight := PagePtToScreen(R.BottomRight);
  if R.Left < 0 then
    with HScroll do
     Position := Position + R.Left
  else if R.Right > ClientWidth then begin
    if R.Right - R.Left > ClientWidth then
      R.Right := R.Left + ClientWidth;
    with HScroll do
      Position := Position + R.Right - ClientWidth;
  end;
  if R.Top < 0 then
    with VScroll do
      Position := Position + R.Top
  else if R.Bottom > ClientHeight then begin
    if R.Bottom - R.Top > ClientHeight then
      R.Bottom := R.Top + ClientHeight;
    with VScroll do
      Position := Position + R.Bottom - ClientHeight;
  end;
end;

procedure TIpHtmlInternalPanel.ScrollInView(R : TRect);
begin
  R.Bottom := R.Top + (ClientHeight - (R.Bottom - R.Top) - 10);
  R.Right := R.Left + (ClientWidth - (R.Right - R.Left) - 10);
  ScrollInViewRaw(R);
end;

procedure TIpHtmlInternalPanel.ScrollPtInView(P : TPoint);
begin
  P := PagePtToScreen(P);
  if P.x < 0 then
    with HScroll do
     Position := Position + P.x
  else if P.x > ClientWidth then begin
    with HScroll do
      Position := Position + P.x - ClientWidth;
  end;
  if P.y < 0 then
    with VScroll do
      Position := Position + P.y
  else if P.y > ClientHeight then begin
    with VScroll do
      Position := Position + P.y - ClientHeight;
  end;
end;

procedure TIpHtmlInternalPanel.ScrollRequest(Sender: TIpHtml; const R: TRect);
begin
  ScrollInView(R);
end;

procedure TIpHtmlInternalPanel.SetHtml(const Value: TIpHtml);
begin
  FHyper := Value;
  InvalidateSize;
end;

procedure TIpHtmlInternalPanel.SetPageRect(const Value: TRect);
begin
  if not SettingPageRect then begin
    SettingPageRect := True;
    FPageRect := Value;
    HScroll.CalcAutoRange;
    VScroll.CalcAutoRange;
    SettingPageRect := False;
  end;
end;

procedure TIpHtmlInternalPanel.UpdateScrollBars;
begin
  if not FUpdatingScrollBars and HandleAllocated then
    try
      FUpdatingScrollBars := True;
      if VScroll.NeedsScrollBarVisible then
      begin
        HScroll.Update(False, True);
        VScroll.Update(True, False);
      end
      else if HScroll.NeedsScrollBarVisible then
      begin
        VScroll.Update(False, True);
        HScroll.Update(True, False);
      end
      else
      begin
        VScroll.Update(False, False);
        HScroll.Update(True, False);
      end;
    finally
      FUpdatingScrollBars := False;
    end;
end;

procedure TIpHtmlInternalPanel.WMHScroll(var Message: TWMHScroll);
begin
  {$IFDEF IP_LAZARUS}
  if HScroll.Visible then
    HScroll.ScrollMessage(Message);
  {$ELSE}
  if (Message.ScrollBar = 0) and HScroll.Visible then
    HScroll.ScrollMessage(Message) else
    inherited;
  {$ENDIF}
end;

procedure TIpHtmlInternalPanel.WMVScroll(var Message: TWMVScroll);
begin
  {$IFDEF IP_LAZARUS}
  if VScroll.Visible then
    VScroll.ScrollMessage(Message);
  {$ELSE}
  if (Message.ScrollBar = 0) and VScroll.Visible then
    VScroll.ScrollMessage(Message) else
    inherited;
  {$ENDIF}
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtmlInternalPanel.AsyncHotInvoke(data: ptrint);
begin
  DoHotInvoke;
end;
{$ENDIF}

procedure TIpHtmlInternalPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TIpHtmlInternalPanel.DoOnMouseWheel(Shift: TShiftState; Delta, XPos,
  YPos: SmallInt);
var
  I : Integer;
begin
  if Delta < 0 then begin
    for I := 1 to WheelDelta do
      Perform(WM_VSCROLL, MAKELONG(SB_LINEDOWN, 0), 0);
  end else if Delta > 0 then begin
    for I := 1 to WheelDelta do
      Perform(WM_VSCROLL, MAKELONG(SB_LINEUP, 0), 0);
  end;

end;

{$IFDEF Version4}
procedure TIpHtmlInternalPanel.MouseWheelHandler(var Message: TMessage);
begin
  inherited;
  with Message do
    DoOnMouseWheel(KeysToShiftState(LOWORD(wParam)),
                   HIWORD(wParam),
                   LOWORD(lParam), HIWORD(lParam));
end;
{$ENDIF}

procedure TIpHtmlInternalPanel.ClearSelection;
begin
  Hyper.SetSelection(Point(-1, -1), Point(-1, -1));
  HaveSelection := False;
end;

procedure TIpHtmlInternalPanel.SetSelection;
begin
  if Hyper <> nil then
    Hyper.SetSelection(SelStart, SelEnd);
end;

function TIpHtmlInternalPanel.HtmlPanel: TIpHtmlCustomPanel;
begin
  Result := TIpHtmlPanel(Parent);
  {$IFDEF IP_LAZARUS}
  while not (Result is TIpHtmlPanel) do
  {$ELSE}
  while (Result.ClassType <> TIpHtmlPanel) do
  {$ENDIF}
    Result := TIpHtmlPanel(Result.Parent);
end;

{ TIpHtmlScrollBar }

constructor TIpHtmlScrollBar.Create(AControl: TIpHtmlInternalPanel;
  AKind: TScrollBarKind);
begin
  inherited Create;
  FControl := AControl;
  FKind := AKind;
  FPageIncrement := 80;
  FIncrement := FPageIncrement div 10;
  FVisible := True;
  {FDelay := 10;}                                                      {!!.12}
  {FLineDiv := 4;}                                                     {!!.12}
  {FPageDiv := 12;}                                                    {!!.12}
  {FColor := clBtnHighlight;}                                          {!!.12}
  {FParentColor := True;}                                              {!!.12}
  FUpdateNeeded := True;
end;

procedure TIpHtmlScrollBar.CalcAutoRange;
begin
  if Kind = sbHorizontal then
    DoSetRange(FControl.PageRect.Right)
  else
    DoSetRange(FControl.PageRect.Bottom);
end;

function TIpHtmlScrollBar.ControlSize(ControlSB, AssumeSB: Boolean): Integer;
var
  BorderAdjust: Integer;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Style: Longint;
  begin
    Style := WS_HSCROLL;
    if Code = SB_VERT then Style := WS_VSCROLL;
    Result := GetWindowLong(FControl.Handle, GWL_STYLE) and Style <> 0;
  end;

  function Adjustment(Code, Metric: Word): Integer;
  begin
    Result := 0;
    if not ControlSB then
      if AssumeSB and not ScrollBarVisible(Code) then
        Result := -(GetSystemMetrics(Metric) - BorderAdjust)
      else if not AssumeSB and ScrollBarVisible(Code) then
        Result := GetSystemMetrics(Metric) - BorderAdjust;
  end;

begin
  BorderAdjust := Integer(GetWindowLong(FControl.Handle, GWL_STYLE) and
    (WS_BORDER or WS_THICKFRAME) <> 0);
  if Kind = sbVertical then
    Result := FControl.ClientHeight + Adjustment(SB_HORZ, SM_CXHSCROLL) else
    Result := FControl.ClientWidth + Adjustment(SB_VERT, SM_CYVSCROLL);
end;

function TIpHtmlScrollBar.NeedsScrollBarVisible: Boolean;
begin
  Result := FRange > ControlSize(False, False);
end;

procedure TIpHtmlScrollBar.ScrollMessage(var Msg: TWMScroll);

  function GetRealScrollPosition: Integer;
  var
    SI: TScrollInfo;
    Code: Integer;
  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_HORZ;
    if FKind = sbVertical then
      Code := SB_VERT;
    Result := Msg.Pos;
    if FlatSB_GetScrollInfo(FControl.Handle, Code, SI) then
      Result := SI.nTrackPos;
  end;

begin
  with Msg do
    case ScrollCode of
    SB_LINEUP:
      SetPosition(FPosition - FIncrement);
    SB_LINEDOWN:
      SetPosition(FPosition + FIncrement);
    SB_PAGEUP:
      SetPosition(FPosition - ControlSize(True, False));
    SB_PAGEDOWN:
      SetPosition(FPosition + ControlSize(True, False));
    SB_THUMBPOSITION:
      if FCalcRange > 32767 then
        SetPosition(GetRealScrollPosition)
      else
        SetPosition(Pos);
    SB_THUMBTRACK:
      if Tracking then
        if FCalcRange > 32767 then
          SetPosition(GetRealScrollPosition)
        else
          SetPosition(Pos);
    SB_TOP:
      SetPosition(0);
    SB_BOTTOM:
      SetPosition(FCalcRange);
    SB_ENDSCROLL:
      ;
    end;
end;

procedure TIpHtmlScrollBar.SetPosition(Value: Integer);
var
  Code: Word;
begin
  if csReading in FControl.ComponentState then
    FPosition := Value
  else begin
    if Value > FCalcRange then Value := FCalcRange
    else if Value < 0 then Value := 0;
    if Kind = sbHorizontal then
      Code := SB_HORZ else
      Code := SB_VERT;
    if Value <> FPosition then
    begin
      FPosition := Value;
      if Kind = sbHorizontal then
        FControl.ViewLeft := Value
      else
        FControl.ViewTop := Value;
      FControl.Invalidate;
    end;
    if FlatSB_GetScrollPos(FControl.Handle, Code) <> FPosition then
      FlatSB_SetScrollPos(FControl.Handle, Code, FPosition, True);
  end;
end;

procedure TIpHtmlScrollBar.DoSetRange(Value: Integer);
begin
  FRange := Value;
  if FRange < 0 then FRange := 0;
  FControl.UpdateScrollBars;
end;

procedure TIpHtmlScrollBar.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FControl.UpdateScrollBars;
end;

procedure TIpHtmlScrollBar.Update(ControlSB, AssumeSB: Boolean);
type
  TPropKind = (pkStyle, pkButtonSize, pkThumbSize, pkSize, pkBkColor);
const
  Props: array[TScrollBarKind, TPropKind] of Integer = (
    (WSB_PROP_HSTYLE, WSB_PROP_CXHSCROLL, WSB_PROP_CXHTHUMB, WSB_PROP_CYHSCROLL,
     WSB_PROP_HBKGCOLOR),
    (WSB_PROP_VSTYLE, WSB_PROP_CYVSCROLL, WSB_PROP_CYVTHUMB, WSB_PROP_CXVSCROLL,
     WSB_PROP_VBKGCOLOR));
  Kinds: array[TScrollBarKind] of Integer = (WSB_PROP_HSTYLE, WSB_PROP_VSTYLE);
var
  Code: Word;
  ScrollInfo: TScrollInfo;

  procedure UpdateScrollProperties(Redraw: Boolean);
  begin
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkStyle], FSB_REGULAR_MODE, Redraw);
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkBkColor],
      ColorToRGB(clBtnHighlight), False);
  end;

begin
  FCalcRange := 0;
  Code := SB_HORZ;
  if Kind = sbVertical then
    Code := SB_VERT;
  if Visible then begin
    FCalcRange := Range - ControlSize(ControlSB, AssumeSB);
    if FCalcRange < 0 then
      FCalcRange := 0;
  end;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  if FCalcRange > 0 then
    ScrollInfo.nMax := Range
  else
    ScrollInfo.nMax := 0;
  ScrollInfo.nPage := ControlSize(ControlSB, AssumeSB) + 1;
  ScrollInfo.nPos := FPosition;
  ScrollInfo.nTrackPos := FPosition;
  UpdateScrollProperties(FUpdateNeeded);
  FUpdateNeeded := False;
  FlatSB_SetScrollInfo(FControl.Handle, Code, ScrollInfo, True);
  SetPosition(FPosition);
  FPageIncrement := (ControlSize(True, False) * 9) div 10;
end;

{ TIpHtmlFocusRect }

constructor TIpHtmlFocusRect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption,
    csOpaque, csReplicatable, csDoubleClicks];
  Width := 65;
  Height := 17;
end;

procedure TIpHtmlFocusRect.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'STATIC');
  with Params do begin
    {$IFNDEF IP_LAZARUS}
    Style := Style or SS_NOTIFY;
    {$ENDIF}
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

{$IFDEF IP_LAZARUS}
procedure TIpHtmlFocusRect.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);
  Anchor.DoOnFocus;
end;

procedure TIpHtmlFocusRect.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);
  Anchor.DoOnBlur;
  {HaveFocus := False;}                                                {!!.12}
end;

{$ELSE}
procedure TIpHtmlFocusRect.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Anchor.DoOnFocus;
end;

procedure TIpHtmlFocusRect.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Anchor.DoOnBlur;
  {HaveFocus := False;}                                                {!!.12}
end;

{$ENDIF}
{ TIpHtmlFrame }

procedure TIpHtmlFrame.InitHtml;
begin
  Html.TextColor := FViewer.TextColor;
  Html.LinkColor := FViewer.LinkColor;
  Html.ALinkColor := FViewer.ALinkColor;
  Html.VLinkColor := FViewer.VLinkColor;
  if FViewer.DataProvider <> nil then
    Html.OnGetImageX := FViewer.DataProvider.DoGetImage;
  Html.OnInvalidateRect := InvalidateRect;
  Html.OnInvalidateSize := InvalidateSize;
  Html.OnGet := Get;
  Html.OnPost := Post;
  Html.OnIFrameCreate := IFrameCreate;
  Html.OnURLCheck := FViewer.URLCheck;
  Html.OnReportURL := FViewer.ReportURL;
  Html.FlagErrors := FFlagErrors;
  Html.MarginWidth := FMarginWidth;
  Html.MarginHeight := FMarginHeight;
end;

constructor TIpHtmlFrame.Create(Viewer: TIpHtmlCustomPanel; Parent: TCustomPanel;
  DataProvider : TIpAbstractHtmlDataProvider; FlagErrors, NoScroll: Boolean;
      MarginWidth, MarginHeight: Integer);
begin
  FNoScroll := NoScroll;
  FParent := Parent;
  FViewer := Viewer;
  FDataProvider := DataProvider;
  Html := TIpHtml.Create;
  Html.FixedTypeface := Viewer.FixedTypeface;                          {!!.10}
  {$IFDEF IP_LAZARUS}
  Html.DefaultTypeFace := Viewer.DefaultTypeFace;
  {$ENDIF}
  FFlagErrors := FlagErrors;
  FMarginWidth := MarginWidth;
  FMarginheight := MarginHeight;
  InitHtml;
end;

destructor TIpHtmlFrame.Destroy;
var
  i : Integer;
begin
  if FramePanel <> nil then                                            {!!.12}
    FramePanel.OnResize := nil;                                        {!!.12}
  for i := 0 to pred(FrameCount) do
    Frames[i].Free;
  if HyperPanel <> nil then begin
    HyperPanel.Hyper := nil;
    HyperPanel.Free;
    HyperPanel := nil;
  end;
  if (FDataProvider <> nil) and (not (csDestroying in FDataProvider.ComponentState)) then
    FDataProvider.DoLeave(Html);
  Html.Free;
  inherited;
end;

procedure TIpHtmlFrame.InvalidateRect(Sender: TIpHtml; const R: TRect);
begin
  if HyperPanel <> nil then
    {$IFDEF IP_LAZARUS}
    LCLIntf.InvalidateRect(HyperPanel.Handle, @R, False);
    {$ELSE}
    Windows.InvalidateRect(HyperPanel.Handle, @R, False);
    {$ENDIF}
end;

procedure TIpHtmlFrame.InvalidateSize(Sender: TObject);
begin
  if HyperPanel <> nil then
    if not InOpen then                                                 {!!.10}
      HyperPanel.InvalidateSize;
end;

procedure TIpHtmlFrame.OpenURL(const URL: string; Delayed: Boolean);
begin
  if Delayed then begin
    FViewer.GetURL := URL;
    FViewer.PostURL := '';
    FViewer.PostData := nil;
    PostMessage(FViewer.Handle, CM_IpHttpGetRequest, 0, PtrInt(Self));
  end else
    OpenRelativeURL(URL);
end;

procedure TIpHtmlFrame.AlignPanels;
var
  ColW : TIntArr;
  RowH : TIntArr;
  ColWCount, RowHCount : Integer;
  N, i, R, C, L, T : Integer;
begin
  if (Html = nil) or (Html.FrameSet = nil) then exit;
  if FramePanel = nil then exit;
  ColW := CalcMultiLength(Html.FrameSet.Cols, FramePanel.ClientWidth,
    ColWCount);{!!.10}
  try
    RowH := CalcMultiLength(Html.FrameSet.Rows, FramePanel.ClientHeight,
      RowHCount); {!!.10}
    try
      R := 0;
      C := 0;
      L := 0;
      T := 0;
      N := 0;
      for i := 0 to pred(Html.FrameSet.ChildCount) do begin
        if Html.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
          if Pnl[N] <> nil then                                            {!!.03}
            Pnl[N].SetBounds(L, T, ColW[C], RowH[R]);
          inc(L, ColW[C]);
          if C < ColWCount - 1 then
            inc(C)
          else begin
            C := 0;
            L := 0;
            inc(T, RowH[R]);
            inc(R);
          end;
          inc(N);
        end;
      end;
    finally
      RowH.Free;
    end;
  finally
    ColW.Free;
  end;
end;

function TIpHtmlFrame.IsExternal(const URL: string): Boolean;
var
  St, ResourceType : string;
begin
  if Assigned(FDataProvider) then
    St := FDataProvider.BuildURL(CurURL, URL)
  else
    St := IpUtils.BuildURL(CurURL, URL);
  if FDataProvider = nil then
    raise EIpHtmlException.Create(SHtmlNoDataProvider);        {!!.02}
  if not FDataProvider.DoCheckURL(St, ResourceType) then
    raise EIpHtmlException.Create(SHtmlResUnavail + St);       {!!.02}
  St := LowerCase(ResourceType);

  if ( Pos('text/', St) = 0) and (pos('image/', St) = 0) then begin
    FViewer.FHotURL := St;
    FViewer.DoHotClick;
    Result := True;
  end else
    Result := False;
end;

function BuildImagePage(const URL:string): TMemoryStream;
var
  S : string;
begin
  Result := TMemoryStream.Create;
  S := '<Html><BODY><IMG src=';
  Result.Write(S[1], length(S));
  Result.Write(URL[1], length(URL));
  S := '></BODY></Html>';
  Result.Write(S[1], length(S));
  Result.Seek(0, 0);
end;

procedure TIpHtmlFrame.OpenRelativeURL(const URL: string);
var
  S : TStream;
  MW, MH,
  i, R, C, L, T : Integer;
  ColW : TIntArr;
  RowH : TIntArr;
  ColWCount, RowHCount : Integer;
  Scroll : Boolean;
  St, ResourceType : string;
  CurFrameDef : TIpHtmlNodeFrame;
  IsImage : Boolean;
begin
  InOpen := True;                                                      {!!.10}
  try                                                                  {!!.10}
    if Assigned(FDataProvider) then
      St := FDataProvider.BuildURL(CurURL, URL)
    else
      St := IpUtils.BuildURL(CurURL, URL);

    if FDataProvider = nil then
      raise EIpHtmlException.Create(SHtmlNoDataProvider);              {!!.02}
    if not FDataProvider.DoCheckURL(St, ResourceType) then
      raise EIpHtmlException.Create(SHtmlResUnavail + St);             {!!.02}
    {if CompareText(St, CurURL) = 0 then exit;}                        {!!.12}
    IsImage := False;
    S := nil;
    if pos('image/', LowerCase(ResourceType)) <> 0 then begin
      IsImage := True;
      S := BuildImagePage(St);
    end else

    if Pos('text/', LowerCase(ResourceType)) = 0 then begin
      FViewer.FHotURL := St;
      FViewer.DoHotClick;
      exit;
    end;
    CurURL := St;
    CurAnchor := '';
    for i := 0 to pred(FrameCount) do
      Frames[i].Free;
    FramePanel.Free;
    FramePanel := nil;
    FrameCount := 0;
    if HyperPanel <> nil then begin
      Html.OnScroll := nil;
      HyperPanel.Hyper := nil;
      HyperPanel.Free;
      HyperPanel := nil;
    end;
    if FDataProvider <> nil then
      FDataProvider.DoLeave(Html);
    Html.Clear;
    ColWCount := 0;
    RowHCount := 0;
    if FDataProvider <> nil then begin
      if not IsImage then
        S := FDataProvider.DoGetHtmlStream(CurURL, PostData);
      if S <> nil then
        try
          Html.CurURL := CurURL;
          Html.LoadFromStream(S);
          if Html.HasFrames then begin
            FramePanel := TPanel.Create(FParent);
            FramePanel.BevelOuter := bvNone;
            FramePanel.Align := alClient;
            FramePanel.Parent := FParent;
            FramePanel.OnResize := FramePanelResize;
            FramePanel.FullRepaint := False;
            ColW := CalcMultiLength(Html.FrameSet.Cols, FramePanel.ClientWidth,
              ColWCount); {!!.10}
            try
              RowH := CalcMultiLength(Html.FrameSet.Rows, FramePanel.ClientHeight,
                RowHCount); {!!.10}
              try
                R := 0;
                C := 0;
                L := 0;
                T := 0;
                FrameCount := 0;
                for i := 0 to pred(Html.FrameSet.ChildCount) do begin
                  if Html.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
                    CurFrameDef := TIpHtmlNodeFrame(Html.FrameSet.ChildNode[i]);
                    Pnl[FrameCount] := TPanel.Create(FramePanel);
                    Pnl[FrameCount].BevelOuter := bvNone;
                    Pnl[FrameCount].SetBounds(L, T, ColW[C], RowH[R]);
                    Pnl[FrameCount].Parent := FramePanel;
                    Pnl[FrameCount].FullRepaint := False;

                    if CurFrameDef.FrameBorder <> 0 then begin                {!!.02}
                      Pnl[FrameCount].BorderStyle := bsSingle;                {!!.02}
                      Pnl[FrameCount].BorderWidth := CurFrameDef.FrameBorder; {!!.02}
                    end;                                                      {!!.02}

                    inc(L, ColW[C]);

                    case CurFrameDef.Scrolling of
                    hfsAuto, hfsYes :
                      Scroll := True;
                    else //hfsNo :
                      Scroll := False;
                    end;

                    if CurFrameDef.MarginWidth <> -1 then
                      MW := CurFrameDef.MarginWidth
                    else
                      MW := FViewer.MarginWidth;
                    if CurFrameDef.MarginHeight <> -1 then
                      MH:= CurFramedef.MarginHeight
                    else
                      MH := FViewer.MarginHeight;

                    Frames[FrameCount] :=
                      TIpHtmlFrame.Create(FViewer, Pnl[FrameCount], FDataProvider,
                        FViewer.FlagErrors, not Scroll, MW, MH);
                    Frames[FrameCount].Name := CurFrameDef.Name;
                    if C < ColWCount - 1 then
                      inc(C)
                    else begin
                      C := 0;
                      L := 0;
                      inc(T, RowH[R]);
                      inc(R);
                    end;
                    inc(FrameCount);
                  end;
                end;
              finally
                RowH.Free;
              end;
            finally
              ColW.Free;
            end;
            Application.ProcessMessages;
            FrameCount := 0;
            for i := 0 to pred(Html.FrameSet.ChildCount) do begin
              if Html.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
                Frames[FrameCount].CurURL := CurURL;
                Frames[FrameCount].OpenRelativeURL({Base,}
                  TIpHtmlNodeFrame(Html.FrameSet.ChildNode[i]).Src);
                inc(FrameCount);
              end;
            end;
          end else begin
            HyperPanel := TIpHtmlInternalPanel.Create(FParent);
            if FNoScroll then begin
              HyperPanel.HScroll.Visible := False;
              HyperPanel.VScroll.Visible := False;
            end;
            HyperPanel.Parent := FParent;
            HyperPanel.Align := alClient;
            HyperPanel.OnHotChange := FViewer.HotChange;
            HyperPanel.OnCurElementChange := FViewer.CurElementChange;
            HyperPanel.OnHotClick := FViewer.HotClick;
            HyperPanel.OnClick := FViewer.ClientClick;
            HyperPanel.TabStop := True;
            Html.OnScroll := HyperPanel.ScrollRequest;
            Html.OnControlClick := ControlClick;
            Html.OnControlCreate := ControlCreate;
            for i := 0 to pred(Html.AnchorList.Count) do
              with TIpHtmlFocusRect.Create(HyperPanel) do begin
                SetBounds(-100, -100, 10, 10);
                TabStop := True;
                Parent := HyperPanel;
                Anchor := Html.AnchorList[i];
              end;
            for i := 0 to pred(Html.ControlList.Count) do
              TIpHtmlNode(Html.ControlList[i]).CreateControl(HyperPanel);
            HyperPanel.Hyper := Html;
          end;
        finally
          S.Free;
        end;
    end;
  finally                                                              {!!.10}
    InOpen := False;                                                   {!!.10}
    if HyperPanel <> nil then
      HyperPanel.InvalidateSize;                                         {!!.10}
  end;                                                                 {!!.10}
end;

procedure TIpHtmlFrame.FramePanelResize(Sender: TObject);
begin
  AlignPanels;
end;

procedure TIpHtmlFrame.MakeAnchorVisible(const URL: string);
var
  E : TIpHtmlNode;
  i : Integer;
begin
  E := Html.FindElement(URL);
  CurAnchor := '';
  if E <> nil then begin
    E.MakeVisible;
    CurAnchor := '#'+URL;
  end else
    for i := 0 to pred(FrameCount) do
      Frames[i].MakeAnchorVisible(URL);
end;

procedure TIpHtmlFrame.Home;
begin
  if Html <> nil then
    Html.Home;
end;

function TIpHtmlFrame.FindFrame(const FrameName: string): TIpHtmlFrame;
var
  i : Integer;
begin
  if AnsiCompareText(FrameName, Name) = 0 then
    Result := Self
  else begin
    Result := nil;
    for i := 0 to pred(FrameCount) do begin
      Result := Frames[i].FindFrame(FrameName);
      if Result <> nil then
        exit;
    end;
  end;
end;

procedure TIpHtmlFrame.Get(Sender: TIpHtml; const URL: string);
begin
  FViewer.GetURL := URL;
  FViewer.PostURL := '';
  FViewer.PostData := nil;
  PostMessage(FViewer.Handle, CM_IpHttpGetRequest, 0, PtrInt(Self));
end;

procedure TIpHtmlFrame.Post(Sender: TIpHtml; const URL:string;
  FormData: TIpFormDataEntity);                                        {!!.12}
begin
  FViewer.GetURL := '';
  FViewer.PostURL := URL;
  FViewer.PostData := FormData;                                        {!!.12}
  PostMessage(FViewer.Handle, CM_IpHttpGetRequest, 0, PtrInt(Self));
end;

function TIpHtmlFrame.HaveSelection: Boolean;
var
  i : Integer;
begin
  if Html = nil then
    Result := False
  else
    if Html.HaveSelection then
      Result := True
    else begin
      Result := False;
      for i := 0 to pred(FrameCount) do
        if Frames[i].HaveSelection then begin
          Result := True;
          break;
        end;
    end;
end;

procedure TIpHtmlFrame.CopyToClipboard;
var
  i : Integer;
begin
  if Html <> nil then
    if Html.HaveSelection then
      Html.CopyToClipboard
    else begin
      for i := 0 to pred(FrameCount) do
        if Frames[i].HaveSelection then begin
          Frames[i].CopyToClipboard;
          exit;
        end;
    end;
end;

procedure TIpHtmlFrame.SelectAll;
var
  i : Integer;
begin
  if Html <> nil then begin
    Html.SelectAll;
    for i := 0 to pred(FrameCount) do
      Frames[i].SelectAll;
  end;
end;

{!!.10 new}
procedure TIpHtmlFrame.DeselectAll;
var
  i : Integer;
begin
  if Html <> nil then begin
    Html.DeselectAll;
    for i := 0 to pred(FrameCount) do
      Frames[i].DeselectAll;
  end;
end;

procedure TIpHtmlFrame.IFrameCreate(Sender: TIpHtml; Parent: TWinControl;
  Frame: TIpHtmlNodeIFRAME; var Control: TWinControl);
var
  MW, MH, W, H : Integer;
  Scroll : Boolean;
  NewFrame : TIpHtmlFrame;
begin
  Control := TPanel.Create(Parent);
  Pnl[FrameCount] := TPanel(Control);
  TPanel(Control).BevelOuter := bvNone;
  case Frame.Width.LengthType of
  hlAbsolute :
    W := Frame.Width.LengthValue;
  else
  {hlUndefined,
  hlPercent :}
    begin
      if Frame.Width.LengthType = hlUndefined then
        W := Parent.ClientWidth
      else
        W := round(Frame.Width.LengthValue * Parent.ClientWidth / 100);
    end;
  end;
  case Frame.Height.LengthType of
  hlAbsolute :
    H := Frame.Height.LengthValue;
  else
  {hlUndefined,
  hlPercent :}
    begin
      if Frame.Height.LengthType = hlUndefined then
        H := Parent.ClientHeight
      else
        H := round(Frame.Height.LengthValue * Parent.ClientHeight / 100);
    end;
  end;
  TPanel(Control).SetBounds(0, 0, W, H);
  TPanel(Control).Parent := Parent;
  TPanel(Control).FullRepaint := False;
  case Frame.Scrolling of
  hfsAuto, hfsYes :
    Scroll := True;
  else //hfsNo :
    Scroll := False;
  end;
  if Frame.FrameBorder <> 0 then begin
    TPanel(Control).BorderStyle := bsSingle;
    TPanel(Control).BorderWidth := Frame.FrameBorder;
  end;

  if Frame.MarginWidth <> -1 then
    MW := Frame.MarginWidth
  else
    MW := FViewer.MarginWidth;
  if Frame.MarginHeight <> -1 then
    MH:= Frame.MarginHeight
  else
    MH := FViewer.MarginHeight;

  NewFrame :=
    TIpHtmlFrame.Create(FViewer, TCustomPanel(Control), FDataProvider,
      FViewer.FlagErrors, not Scroll, MW, MH);
  Frames[FrameCount] := NewFrame;
  NewFrame.Name := Frame.Name;
  Application.ProcessMessages;
  NewFrame.CurURL := CurURL;
  NewFrame.OpenRelativeURL(Frame.Src);
  inc(FrameCount);
  Frame.FFrame := NewFrame;
end;

procedure TIpHtmlFrame.SetHtml(NewHtml: TIpHtml);
var
  MW, MH,
  ColWCount, RowHCount,
  i, R, C, L, T : Integer;
  ColW : TIntArr;
  RowH : TIntArr;
  Scroll : Boolean;
  CurFrameDef : TIpHtmlNodeFrame;
begin
  for i := 0 to pred(FrameCount) do
    Frames[i].Free;
  FramePanel.Free;
  FramePanel := nil;
  FrameCount := 0;
  if HyperPanel <> nil then begin
    Html.OnScroll := nil;
    HyperPanel.Hyper := nil;
    HyperPanel.Free;
    HyperPanel := nil;
  end;
  if FDataProvider <> nil then
    FDataProvider.DoLeave(Html);
  Html.Clear;
  ColWCount := 0;
  RowHCount := 0;
  Html.Free;
  Html := NewHtml;
  InitHtml;
  Html.DoneLoading := True;
  if Html.HasFrames then begin
    FramePanel := TPanel.Create(FParent);
    FramePanel.BevelOuter := bvNone;
    FramePanel.Align := alClient;
    FramePanel.Parent := FParent;
    FramePanel.OnResize := FramePanelResize;
    FramePanel.FullRepaint := False;
    ColW := CalcMultiLength(Html.FrameSet.Cols, FramePanel.ClientWidth,
      ColWCount); {!!.10}
    try
      RowH := CalcMultiLength(Html.FrameSet.Rows, FramePanel.ClientHeight,
        RowHCount); {!!.10}
      try
        R := 0;
        C := 0;
        L := 0;
        T := 0;
        FrameCount := 0;
        for i := 0 to pred(Html.FrameSet.ChildCount) do begin
          if Html.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
            CurFrameDef := TIpHtmlNodeFrame(Html.FrameSet.ChildNode[i]);
            Pnl[FrameCount] := TPanel.Create(FramePanel);
            Pnl[FrameCount].BevelOuter := bvNone;
            Pnl[FrameCount].SetBounds(L, T, ColW[C], RowH[R]);
            Pnl[FrameCount].Parent := FramePanel;
            Pnl[FrameCount].FullRepaint := False;

            if CurFrameDef.FrameBorder <> 0 then begin                {!!.02}
              Pnl[FrameCount].BorderStyle := bsSingle;                {!!.02}
              Pnl[FrameCount].BorderWidth := CurFrameDef.FrameBorder; {!!.02}
            end;                                                      {!!.02}
        
            inc(L, ColW[C]);

            case CurFrameDef.Scrolling of
            hfsAuto, hfsYes :
              Scroll := True;
            else //hfsNo :
              Scroll := False;
            end;

            if CurFrameDef.MarginWidth <> -1 then
              MW := CurFrameDef.MarginWidth
            else
              MW := FViewer.MarginWidth;
            if CurFrameDef.MarginHeight <> -1 then
              MH:= CurFramedef.MarginHeight
            else
              MH := FViewer.MarginHeight;

            Frames[FrameCount] :=
              TIpHtmlFrame.Create(FViewer, Pnl[FrameCount], FDataProvider,
                FViewer.FlagErrors, not Scroll, MW, MH);
            Frames[FrameCount].Name := CurFrameDef.Name;
            if C < ColWCount - 1 then
              inc(C)
            else begin
              C := 0;
              L := 0;
              inc(T, RowH[R]);
              inc(R);
            end;
            inc(FrameCount);
          end;
        end;
      finally
        RowH.Free;
      end;
    finally
      ColW.Free;
    end;
    Application.ProcessMessages;
    FrameCount := 0;
    for i := 0 to pred(Html.FrameSet.ChildCount) do begin
      if Html.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
        Frames[FrameCount].CurURL := CurURL;
        Frames[FrameCount].OpenRelativeURL(
          TIpHtmlNodeFrame(Html.FrameSet.ChildNode[i]).Src);
        inc(FrameCount);
      end;
    end;
  end else begin
    HyperPanel := TIpHtmlInternalPanel.Create(FParent);
    if FNoScroll then begin
      HyperPanel.HScroll.Visible := False;
      HyperPanel.VScroll.Visible := False;
    end;
    HyperPanel.Parent := FParent;
    HyperPanel.Align := alClient;
    HyperPanel.OnHotChange := FViewer.HotChange;
    HyperPanel.OnCurElementChange := FViewer.CurElementChange;
    HyperPanel.OnHotClick := FViewer.HotClick;
    HyperPanel.OnClick := FViewer.ClientClick;
    HyperPanel.TabStop := True;
    Html.OnScroll := HyperPanel.ScrollRequest;
    for i := 0 to pred(Html.AnchorList.Count) do
      with TIpHtmlFocusRect.Create(HyperPanel) do begin
        SetBounds(-100, -100, 10, 10);
        TabStop := True;
        Parent := HyperPanel;
        Anchor := Html.AnchorList[i];
      end;
    for i := 0 to pred(Html.ControlList.Count) do begin
      TIpHtmlNode(Html.ControlList[i]).CreateControl(HyperPanel);
    end;
    HyperPanel.Hyper := Html;
  end;
end;

procedure TIpHtmlFrame.EnumDocuments(Enumerator: TIpHtmlEnumerator);
var
  i : Integer;
begin
  if Html <> nil then
    Enumerator(Html);
  for i := 0 to pred(FrameCount) do
    Frames[i].EnumDocuments(Enumerator);
end;

procedure TIpHtmlFrame.ControlClick(Sender: TIpHtml;
  Node: TIpHtmlNodeControl);
begin
  FViewer.ControlClick(Self, Sender, Node);
end;

procedure TIpHtmlFrame.ControlCreate(Sender: TIpHtml;
  Node: TIpHtmlNodeControl);
begin
  FViewer.ControlCreate(Self, Sender, Node);
end;

procedure TIpHtmlFrame.Scroll(Action: TIpScrollAction);
var
  R : TRect;
  H, W : Integer;
begin
  if Html = nil then exit;
  if HyperPanel = nil then exit;
  R := Html.PageViewRect;
  H := R.Bottom - R.Top;
  W := R.Right - R.Left;
  case Action of
  hsaHome :
    begin
      R.Top := 0;
      R.Bottom := R.Top + H;
    end;
  hsaEnd :
    begin
      R.Bottom := Html.FPageRect.Bottom;
      R.Top := R.Bottom - H;
    end;
  hsaPgUp :
    begin
      OffsetRect(R, 0, -H);
      if R.Top < 0 then begin
        R.Top := 0;
        R.Bottom := R.Top + H;
      end;
    end;
  hsaPgDn :
    begin
      OffsetRect(R, 0, H);
      if R.Bottom > Html.FPageRect.Bottom then begin
        R.Bottom := Html.FPageRect.Bottom;
        R.Top := R.Bottom - H;
      end;
    end;
  hsaLeft :
    begin
      OffsetRect(R, -100, 0);
      if R.Left < 0 then begin
        R.Left := 0;
        R.Right := R.Left + W;
      end;
    end;
  hsaRight :
    begin
      OffsetRect(R, 100, 0);
      if R.Right > Html.FPageRect.Right then begin
        R.Bottom := Html.FPageRect.Right;
        R.Left := R.Right - W;
      end;
    end;
  hsaUp :
    begin
      OffsetRect(R, 0, -100);
      if R.Top < 0 then begin
        R.Top := 0;
        R.Bottom := R.Top + H;
      end;
    end;
  hsaDown :
    begin
      OffsetRect(R, 0, 100);
      if R.Bottom > Html.FPageRect.Bottom then begin
        R.Bottom := Html.FPageRect.Bottom;
        R.Top := R.Bottom - H;
      end;
    end;
  end;
  HyperPanel.ScrollInViewRaw(R);
end;

procedure TIpHtmlFrame.Stop;
begin
  if FDataProvider <> nil then
    FDataProvider.DoLeave(Html);
end;

{ TIpHtmlNvFrame }

procedure TIpHtmlNvFrame.InitHtml;
begin
  if FScanner.DataProvider <> nil then
    Html.OnGetImageX := FScanner.DataProvider.DoGetImage;
  Html.FlagErrors := FFlagErrors;
end;

constructor TIpHtmlNvFrame.Create(Scanner: TIpHtmlCustomScanner;
  DataProvider : TIpAbstractHtmlDataProvider; FlagErrors: Boolean);
begin
  FScanner := Scanner;
  FDataProvider := DataProvider;
  Html := TIpHtml.Create;
  FFlagErrors := FlagErrors;
  InitHtml;
end;

destructor TIpHtmlNvFrame.Destroy;
var
  i : Integer;
begin
  for i := 0 to pred(FrameCount) do
    Frames[i].Free;
  Html.Free;
  inherited;
end;

procedure TIpHtmlNvFrame.OpenURL(const URL: string);
begin
  OpenRelativeURL(URL);
end;

procedure TIpHtmlNvFrame.OpenRelativeURL(const {Base, }URL: string);
var
  S : TStream;
  i, C : Integer;
  ColWCount : Integer;
  St, ResourceType : string;
  CurFrameDef : TIpHtmlNodeFrame;
begin
  if Assigned(FDataProvider) then
    St := FDataProvider.BuildURL(CurURL, URL)
  else
    St := IpUtils.BuildURL(CurURL, URL);

  if FDataProvider = nil then
    raise EIpHtmlException.Create(SHtmlNoDataProvider);        {!!.02}
  if not FDataProvider.DoCheckURL(St, ResourceType) then
    raise EIpHtmlException.Create(SHtmlResUnavail + St);       {!!.02}
  if CompareText(ResourceType, 'text/html') <> 0 then
    exit;
  if CompareText(St, CurURL) = 0 then exit;
  CurURL := St;
  CurAnchor := '';
  for i := 0 to pred(FrameCount) do
    Frames[i].Free;
  FrameCount := 0;
  FDataProvider.DoLeave(Html);
  Html.Clear;
  ColWCount := 0;
  if FDataProvider <> nil then begin
    S := FDataProvider.DoGetHtmlStream(CurURL, PostData);
    if S <> nil then
      try
        Html.CurURL := CurURL;
        Html.LoadFromStream(S);
        if Html.HasFrames then begin
          C := 0;
          FrameCount := 0;
          for i := 0 to pred(Html.FrameSet.ChildCount) do begin
            if Html.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
              CurFrameDef := TIpHtmlNodeFrame(Html.FrameSet.ChildNode[i]);
              Frames[FrameCount] :=
                TIpHtmlNvFrame.Create(FScanner, FDataProvider,
                  FScanner.FlagErrors);
              Frames[FrameCount].Name := CurFrameDef.Name;
              if C < ColWCount - 1 then
                inc(C)
              else begin
                C := 0;
              end;
              inc(FrameCount);
            end;
          end;
          Application.ProcessMessages;
          FrameCount := 0;
          for i := 0 to pred(Html.FrameSet.ChildCount) do begin
            if Html.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
              Frames[FrameCount].CurURL := CurURL;
              Frames[FrameCount].OpenRelativeURL({Base,}
                TIpHtmlNodeFrame(Html.FrameSet.ChildNode[i]).Src);
              inc(FrameCount);
            end;
          end;
        end;
      finally
        S.Free;
      end;
  end;
end;

procedure TIpHtmlNvFrame.MakeAnchorVisible(const URL: string);
var
  E : TIpHtmlNode;
  i : Integer;
begin
  E := Html.FindElement(URL);
  CurAnchor := '';
  if E <> nil then begin
    E.MakeVisible;
    CurAnchor := '#'+URL;
  end else
    for i := 0 to pred(FrameCount) do
      Frames[i].MakeAnchorVisible(URL);
end;

procedure TIpHtmlNvFrame.Home;
begin
  if Html <> nil then
    Html.Home;
end;

function TIpHtmlNvFrame.FindFrame(const FrameName: string): TIpHtmlNvFrame;
var
  i : Integer;
begin
  if AnsiCompareText(FrameName, Name) = 0 then
    Result := Self
  else begin
    Result := nil;
    for i := 0 to pred(FrameCount) do begin
      Result := Frames[i].FindFrame(FrameName);
      if Result <> nil then
        exit;
    end;
  end;
end;

function TIpHtmlNvFrame.HaveSelection: Boolean;
var
  i : Integer;
begin
  if Html = nil then
    Result := False
  else
    if Html.HaveSelection then
      Result := True
    else begin
      Result := False;
      for i := 0 to pred(FrameCount) do
        if Frames[i].HaveSelection then begin
          Result := True;
          break;
        end;
    end;
end;

procedure TIpHtmlNvFrame.CopyToClipboard;
var
  i : Integer;
begin
  if Html <> nil then
    if Html.HaveSelection then
      Html.CopyToClipboard
    else begin
      for i := 0 to pred(FrameCount) do
        if Frames[i].HaveSelection then begin
          Frames[i].CopyToClipboard;
          exit;
        end;
    end;
end;

procedure TIpHtmlNvFrame.SelectAll;
var
  i : Integer;
begin
  if Html <> nil then begin
    Html.SelectAll;
    for i := 0 to pred(FrameCount) do
      Frames[i].SelectAll;
  end;
end;

procedure TIpHtmlNvFrame.EnumDocuments(Enumerator: TIpHtmlEnumerator);
var
  i : Integer;
begin
  if Html <> nil then
    Enumerator(Html);
  for i := 0 to pred(FrameCount) do
    Frames[i].EnumDocuments(Enumerator);
end;

procedure TIpHtmlNVFrame.Stop;
begin
  if FDataProvider <> nil then
    FDataProvider.DoLeave(Html);
end;

{ TIpHtmlCustomPanel }

procedure TIpHtmlCustomPanel.DoHotChange;
begin
  if Assigned(FHotChange) then
    FHotChange(Self);
end;

procedure TIpHtmlCustomPanel.DoHotClick;
begin
  if Assigned(FHotClick) then
    FHotClick(Self);
end;

{New in !!.16}
procedure TIpHtmlCustomPanel.DoOnMouseWheel(Shift: TShiftState; Delta, XPos, YPos: SmallInt);
var
  I: Integer;
begin
  if Delta < 0 then
  begin
    for I := 1 to WheelDelta do
      Scroll(hsaDown);
  end else
  if Delta > 0 then
  begin
    for I := 1 To WheelDelta do
      Scroll(hsaUp);
  end;
end;
{!!.16}

procedure TIpHtmlCustomPanel.HotChange(Sender: TObject);
var
  P : TIpHtmlInternalPanel;
  Html : TIpHtml;
begin
  P := TIpHtmlInternalPanel(Sender);
  Html := P.Hyper;
  if Html.HotNode <> nil then begin
    if Html.HotPoint.x >= 0 then
      FHotURL := TIpHtmlNodeA(Html.HotNode).HRef+
        '?'+IntToStr(Html.HotPoint.x)+','+IntToStr(Html.HotPoint.y)
    else
      if Html.HotNode is TIpHtmlNodeA then
       FHotURL := TIpHtmlNodeA(Html.HotNode).HRef
      else
       FHotURL := TIpHtmlNodeAREA(Html.HotNode).HRef;
    FHotNode := Html.HotNode;
    P.Cursor := crHandPoint;
  end else begin
    FHotNode := nil;
    FHotURL := '';
    P.Cursor := crDefault;
  end;
  DoHotChange;
end;

procedure TIpHtmlCustomPanel.CurElementChange(Sender: TObject);
var
  P : TIpHtmlInternalPanel;
  Html : TIpHtml;
begin
  P := TIpHtmlInternalPanel(Sender);
  Html := P.Hyper;
  FCurElement := Html.CurElement;
  if assigned(FCurElementChange) then                                  {!!.10}
    FCurElementChange(Self);                                           {!!.10}
end;

function TIpHtmlCustomPanel.GetTitle: string;
begin
  if (MasterFrame <> nil)
  and (MasterFrame.Html <> nil)
  and (MasterFrame.Html.TitleNode <> nil) then
    Result := MasterFrame.Html.TitleNode.Title
  else
    Result := '';
end;

constructor TIpHtmlCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  BevelOuter := bvNone;
  Caption := '';
  ControlStyle := [csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  TargetStack := TStringList.Create;
  URLStack := TStringList.Create;
  VisitedList := TStringList.Create;
  VisitedList.Sorted := True;
  FTextColor := clBlack;
  FLinkColor := clBlue;
  FVLinkColor := clMaroon;
  FAlinkColor := clRed;
  FShowHints := True;
  FMarginWidth := 10;
  FMarginHeight := 10;
  FAllowTextSelect := True;
  FixedTypeface := 'Courier New';                                      {!!.10}
  {$IFDEF IP_LAZARUS}
  DefaultTypeFace := Graphics.DefFontData.Name;
  {$ENDIF}
  FPrintSettings := TIpHtmlPrintSettings.Create;                       {!!.10}
end;

destructor TIpHtmlCustomPanel.Destroy;
begin
  FPrintSettings.Free;                                                 {!!.10}
  TargetStack.Free;
  URLStack.Free;
  MasterFrame.Free;
  MasterFrame := nil;
  VisitedList.Free;
  inherited;
end;

procedure TIpHtmlCustomPanel.EraseBackground(DC: HDC);
begin
  //
end;

procedure TIpHtmlCustomPanel.OpenURL(const URL: string);
begin
  InternalOpenURL('', URL);
end;

procedure TIpHtmlCustomPanel.MakeAnchorVisible(const Name: string);
begin
  if MasterFrame <> nil then
    MasterFrame.MakeAnchorVisible(Name)
end;

procedure TIpHtmlCustomPanel.InternalOpenURL(const Target, HRef : string);
var
  URL, BaseURL, RelURL : string;
  P : Integer;
  TargetFrame : TIpHtmlFrame;
begin
  if HRef = '' then                                                    {!!.12}
    Exit;                                                              {!!.12}
  if HRef[1] = '#' then begin
    RelURL := copy(HRef, 2, length(HRef) - 1);
    BaseURL := '';
  end else begin
    if MasterFrame <> nil then begin
      if Assigned(FDataProvider) then
        URL := FDataProvider.BuildURL(MasterFrame.Html.CURURL, HRef)
      else
        URL := IpUtils.BuildURL(MasterFrame.Html.CURURL, HRef);
    end
    else
      URL := HRef;
    P := CharPos('#', URL);
    if P = 0 then begin
      RelURL := '';
      BaseURL := URL;
    end else begin
      BaseURL := copy(URL, 1, P - 1);
      RelURL := copy(URL, P + 1, length(URL));
    end;
  end;
  if BaseURL <> '' then begin
    if VisitedList.IndexOf(BaseURL) = -1 then
      VisitedList.Add(BaseURL);
    if (Target <> '') and (MasterFrame <> nil) then
      TargetFrame := MasterFrame.FindFrame(Target)
    else
      TargetFrame := nil;
    if TargetFrame = nil then begin
      if MasterFrame <> nil then
        Push('', MasterFrame.CURURL + MasterFrame.CurAnchor);
      if DataProvider = nil then
        raise EIpHtmlException.Create(SHtmlNoDataProvider);    {!!.02}
      if (MasterFrame = nil)
      or not MasterFrame.IsExternal(URL) then begin
        if (MasterFrame <> nil)
        and (MasterFrame.Html <> nil) then
          FDataProvider.DoLeave(MasterFrame.Html);
        MasterFrame.Free;
        MasterFrame := nil;
        Application.ProcessMessages;
        MasterFrame := TIpHtmlFrame.Create(Self, Self, DataProvider, FlagErrors, False,
          MarginWidth, MarginHeight);
        // LazDebug try
          MasterFrame.OpenURL(URL, False);
        { LazDebug except
          MasterFrame.Free;
          MasterFrame := nil;
          raise;
        end;}
        {CurURL := URL;}                                               {!!.12}
      end;
    end else begin
      Push(Target, TargetFrame.CURURL +  TargetFrame.CurAnchor);
      TargetFrame.OpenURL(BaseURL, False);
    end;
  end;
  if RelURL <> '' then
    MasterFrame.MakeAnchorVisible(RelURL)
  else
    if MasterFrame <> nil then                                         {!!.02}
      MasterFrame.Home;
  if assigned(FDocumentOpen) then                                      {!!.10}
    FDocumentOpen(Self);                                               {!!.10}
end;

procedure TIpHtmlCustomPanel.HotClick(Sender: TObject);
var
  HRef : string;
  Target : string;
begin
  if TIpHtml(Sender).HotNode is TIpHtmlNodeA then begin
    HRef := TIpHtmlNodeA(TIpHtml(Sender).HotNode).HRef;
    Target := TIpHtmlNodeA(TIpHtml(Sender).HotNode).Target;
  end else begin
    HRef := TIpHtmlNodeAREA(TIpHtml(Sender).HotNode).HRef;
    Target := TIpHtmlNodeAREA(TIpHtml(Sender).HotNode).Target;
  end;
  if (FDataProvider <> nil)
  and FDataProvider.CanHandle(HRef) then
    InternalOpenURL(Target, HRef)
  else
    DoHotClick;
end;

procedure TIpHtmlCustomPanel.GoBack;
begin
  if (URLStack.Count > 0) then begin
    {$IFDEF IP_LAZARUS}
    if URLStack.Count >= URLStack.count then Stp := URLStack.Count - 1;
    if URLStack.Count > 0 then begin
      InternalOpenURL(TargetStack[Stp], URLStack[Stp]);
      dec(Stp);
    end;
    {$ELSE}
    InternalOpenURL(TargetStack[Stp], URLStack[Stp]);
    dec(Stp);
    {$ENDIF}
  end;
end;

{$IFDEF IP_LAZARUS}
function TIpHtmlCustomPanel.canGoBack : boolean;
begin
  result := (URLStack.Count > 0);
end;
{$ENDIF}

procedure TIpHtmlCustomPanel.GoForward;
begin
  if Stp < URLStack.Count - 1 then begin
    InternalOpenURL(TargetStack[Stp + 1], URLStack[Stp + 1]);
    inc(Stp);
  end;
end;

{$IFDEF IP_LAZARUS}
function TIpHtmlCustomPanel.canGoForward : boolean;
begin
  result := (Stp < URLStack.Count - 1);
end;
{$ENDIF}

procedure TIpHtmlCustomPanel.Push(const Target, URL: string);
begin
  if (Stp > 0)
  and (TargetStack[Stp] = Target)
  and (URLStack[Stp] = URL) then exit;
  while STP < URLStack.Count - 1 do begin
    URLStack.Delete(Stp);
    TargetStack.Delete(Stp);
  end;
  URLStack.Add(URL);
  TargetStack.Add(Target);
  Stp := URLStack.Count - 1;
end;

procedure TIpHtmlCustomPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) then
    if (AComponent = DataProvider) then begin
      DataProvider := nil;
    end;
  inherited Notification(AComponent, Operation);
end;

procedure TIpHtmlCustomPanel.Paint;
var
  Sz: TSize;
begin
  if csDesigning in ComponentState then begin
    Canvas.Brush.Color := clBtnFace;                                   {!!.10}
    Canvas.FillRect(Canvas.ClipRect);                                  {!!.10}
    Canvas.Pen.Color := clWhite;
    Sz := Canvas.TextExtent('Html');
    Canvas.Polygon([
      Point(0,4),
      Point(0, Height - 5),
      Point(Width div 2 - Sz.cx div 2, Height div 2)]);
    Canvas.Polygon([
      Point(Width - 1,4),
      Point(Width - 1, Height - 5),
      Point(Width div 2 + Sz.cx div 2, Height div 2)]);
    Canvas.Polygon([
      Point(2, 4),
      Point(Width - 3, 4),
      Point(Width div 2, Height div 2 - Sz.cy div 2)]);
    Canvas.Polygon([
      Point(2, Height - 4),
      Point(Width - 3, Height - 4),
      Point(Width div 2, Height div 2 + Sz.cy div 2)]);
    Canvas.Brush.Color := clRed;
    Canvas.Pen.Color := clBlack;
    Canvas.Ellipse(
           Width div 2 - Sz.cx, Height div 2 - Sz.cy,
           Width div 2 + Sz.cx, Height div 2 + Sz.cy);
    Canvas.TextOut(Width div 2 - Sz.cx div 2, Height div 2 - Sz.cy div 2,
      'Html');
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clBlack;
  end;
end;

procedure TIpHtmlCustomPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if (MasterFrame = nil)
  or (MasterFrame.Html = nil)
  or (not MasterFrame.Html.CanPaint) then
    if not (csDesigning in ComponentState) then
      FillRect(Message.DC, ClientRect, Brush.Handle);
  Message.Result := 1;
end;

procedure TIpHtmlCustomPanel.CMIpHttpGetRequest(var Message: TMessage);
var
  FB : TIpHtmlFrame;
begin
  FB := TIpHtmlFrame(Message.lParam);
  if PostData <> nil then begin  {!!.12}
    FB.PostData := PostData;
    FB.OpenRelativeURL(PostURL); {!!.12}
    {$IFNDEF HtmlWithoutHttp}
    PostData.Free; {!!.12}
    PostData := nil; {!!.12}
    {$ENDIF}
  end else                       {!!.12}
    FB.OpenRelativeURL(GetURL);
end;

procedure TIpHtmlCustomPanel.ClientClick(Sender: TObject);
begin
  Click;
end;

function TIpHtmlCustomPanel.HaveSelection: Boolean;
begin
  Result :=
    (MasterFrame <> nil)
    and (MasterFrame.HaveSelection);
end;

procedure TIpHtmlCustomPanel.SelectAll;
begin
  if MasterFrame <> nil then begin
    MasterFrame.SelectAll;
    Invalidate;
  end;
end;

procedure TIpHtmlCustomPanel.DeselectAll;
begin
  if MasterFrame <> nil then begin
    MasterFrame.DeselectAll;
    Invalidate;
  end;
end;

procedure TIpHtmlCustomPanel.CopyToClipboard;
begin
  if MasterFrame <> nil then
    MasterFrame.CopyToClipboard;
end;

procedure TIpHtmlCustomPanel.SetHtml(NewHtml: TIpHtml);
begin
  if (MasterFrame <> nil)
  and (MasterFrame.Html <> nil)
  and (FDataProvider <> nil) then
    FDataProvider.DoLeave(MasterFrame.Html);
  MasterFrame.Free;
  MasterFrame := nil;
  MasterFrame := TIpHtmlFrame.Create(Self, Self, DataProvider, FlagErrors, False,
    MarginWidth, MarginHeight);
  // LazDebug try
    if NewHtml <> nil then
      MasterFrame.SetHtml(NewHtml);
  { LazDebug
  except
    MasterFrame.Free;
    MasterFrame := nil;
    raise;
  end;}
end;

procedure TIpHtmlCustomPanel.URLCheck(Sender: TIpHtml; const URL: string;
  var Visited: Boolean);
begin
  Visited := VisitedList.IndexOf(URL) <> -1;
end;

procedure TIpHtmlCustomPanel.ReportURL(Sender: TIpHtml; const URL: string);
begin
  if (FDataProvider <> nil) then
    FDataProvider.DoReference(URL);
end;

procedure TIpHtmlCustomPanel.EnumDocuments(Enumerator: TIpHtmlEnumerator);
begin
  if MasterFrame <> nil then
    MasterFrame.EnumDocuments(Enumerator);
end;

procedure TIpHtmlCustomPanel.ControlClick(Frame: TIpHtmlFrame; Html: TIpHtml;
  Node: TIpHtmlNodeControl);
begin
  if assigned(FControlClick) then
    FControlClick(Self, Frame, Html, Node);
end;

procedure TIpHtmlCustomPanel.ControlCreate(Frame: TIpHtmlFrame; Html: TIpHtml;
  Node: TIpHtmlNodeControl);
begin
  if assigned(FControlCreate) then
    FControlCreate(Self, Frame, Html, Node);
end;

function TIpHtmlCustomPanel.IsURLHtml(const URL: string): Boolean;
var
  ResourceType: string;
begin
  Result := (FDataProvider <> nil)
  and FDataProvider.DoCheckURL(URL, ResourceType)
  and (CompareText(ResourceType, 'text/html') = 0);
end;

procedure TIpHtmlCustomPanel.Stop;
begin
  if assigned(MasterFrame) then
    MasterFrame.Stop;
end;

{New in !!.16}
{$IFDEF VERSION4}
procedure TIpHtmlCustomPanel.MouseWheelHandler(var Message: TMessage);
begin
  inherited MouseWheelHandler(Message);
  with Message do
    DoOnMouseWheel(KeysToShiftState(LOWORD(wParam)), HIWORD(wParam), LOWORD(lParam), HIWORD(lParam));
end;
{$ENDIF}

function TIpHtmlCustomPanel.GetPrintPageCount: Integer;
begin
  if Assigned(MasterFrame)
  and Assigned(MasterFrame.HyperPanel) then begin
    { !!.10 logic moved to InternalPanel
    Printer.BeginDoc;
    try
      ScaleBitmaps := True;
      GetRelativeAspect(Printer.Canvas.Handle);
    }
      Result := MasterFrame.HyperPanel.GetPrintPageCount;
    {
      !!.10 logic moved to InternalPanel
    finally
      ScaleBitmaps := False;
      Printer.Abort;
      MasterFrame.HyperPanel.InvalidateSize;
    end;
    }
  end else
    Result := 0;
end;

procedure TIpHtmlCustomPanel.Print(FromPg, ToPg: LongInt);
begin
  if Assigned(MasterFrame) then
    MasterFrame.HyperPanel.PrintPages(FromPg, ToPg);
end;

procedure TIpHtmlCustomPanel.PrintPreview;
begin
  {$IFDEF IP_LAZARUS}
  if not assigned(printer) then begin
    raise exception.create(
      'Printer has not been assigned, checkout that package'#13+
      'Printer4lazarus.lpk has been installed and OSPrinters'#13+
      'or PrintDialog is in uses clause of main unit');
  end;
  {$ENDIF}
  if Assigned(MasterFrame) then
    MasterFrame.HyperPanel.PrintPreview;
end;

procedure TIpHtmlCustomPanel.Scroll(Action: TIpScrollAction);
begin
  if MasterFrame <> nil then
    MasterFrame.Scroll(Action);
end;                    

procedure TIpHtmlCustomPanel.WMGetDlgCode(var Msg: TMessage);
begin
  { we want 'em all!  For Lazarus: Then use OnKeyDown! }
  Msg.Result := DLGC_WANTALLKEYS +
                DLGC_WANTARROWS +
                DLGC_WANTCHARS +
                {$IFNDEF IP_LAZARUS}
                DLGC_WANTMESSAGE +
                {$ENDIF}
                DLGC_WANTTAB
end;

function TIpHtmlCustomPanel.GetVersion : string;
begin
  Result := IpShortVersion;
end;

procedure TIpHtmlCustomPanel.SetVersion(const Value : string);
begin
  { Intentionally empty }
end;
{$IFDEF IP_LAZARUS}
procedure TIpHtmlCustomPanel.SetDefaultTypeFace(const Value: string);
begin
  if FDefaultTypeFace<>Value then begin
    FDefaultTypeFace := Value;
    if (MasterFrame<>nil)and(MasterFrame.Html<>nil) then begin
      MasterFrame.Html.DefaultTypeFace := FDefaultTypeFace;
      invalidate;
    end;
  end;
end;
{$ENDIF}

{ TIpHtmlCustomScanner }

function TIpHtmlCustomScanner.GetTitle: string;
begin
  if (MasterFrame <> nil)
  and (MasterFrame.Html <> nil)
  and (MasterFrame.Html.TitleNode <> nil) then
    Result := MasterFrame.Html.TitleNode.Title
  else
    Result := '';
end;

constructor TIpHtmlCustomScanner.Create(AOwner: TComponent);
begin
  inherited;
  TargetStack := TStringList.Create;
  URLStack := TStringList.Create;
end;

destructor TIpHtmlCustomScanner.Destroy;
begin
  TargetStack.Free;
  URLStack.Free;
  MasterFrame.Free;
  MasterFrame := nil;
  inherited;
end;

procedure TIpHtmlCustomScanner.OpenURL(const URL: string);
begin
  InternalOpenURL('', URL);
end;

procedure TIpHtmlCustomScanner.InternalOpenURL(const Target, HRef : string);
var
  URL, BaseURL, RelURL : string;
  P : Integer;
  TargetFrame : TIpHtmlNvFrame;
begin
  if HRef = '' then                                                    {!!.12}
    Exit;                                                              {!!.12}
  if HRef[1] = '#' then begin
    RelURL := copy(HRef, 2, length(HRef) - 1);
    BaseURL := '';
  end else begin
    if MasterFrame <> nil then begin
      if Assigned(FDataProvider) then
        URL := FDataProvider.BuildURL(MasterFrame.Html.CURURL, HRef)
      else
        URL := IpUtils.BuildURL(MasterFrame.Html.CURURL, HRef);
    end
    else
      URL := HRef;
    P := CharPos('#', URL);
    if P = 0 then begin
      RelURL := '';
      BaseURL := URL;
    end else begin
      BaseURL := copy(URL, 1, P - 1);
      RelURL := copy(URL, P + 1, length(URL));
    end;
  end;
  if BaseURL <> '' then begin
    if (Target <> '') and (MasterFrame <> nil) then
      TargetFrame := MasterFrame.FindFrame(Target)
    else
      TargetFrame := nil;
    if TargetFrame = nil then begin
      if MasterFrame <> nil then
        Push('', MasterFrame.CURURL + MasterFrame.CurAnchor);
      if DataProvider = nil then
        raise EIpHtmlException.Create(SHtmlNoDataProvider);    {!!.02}
      if (MasterFrame <> nil)
      and (MasterFrame.Html <> nil) then
        FDataProvider.DoLeave(MasterFrame.Html);
      MasterFrame.Free;
      MasterFrame := nil;
      Application.ProcessMessages;
      MasterFrame := TIpHtmlNVFrame.Create(Self, DataProvider, FlagErrors);
      // LazDebug try
        MasterFrame.OpenURL(URL);
      { LazDebug except
        MasterFrame.Free;
        MasterFrame := nil;
        raise;
      end;}
      CurURL := URL;
    end else begin
      Push(Target, TargetFrame.CURURL +  TargetFrame.CurAnchor);
      TargetFrame.OpenURL(BaseURL);
    end;
  end;
  if RelURL <> '' then
    MasterFrame.MakeAnchorVisible(RelURL)
  else
    MasterFrame.Home;
end;

procedure TIpHtmlCustomScanner.Push(const Target, URL: string);
begin
  if (Stp > 0)
  and (TargetStack[Stp] = Target)
  and (URLStack[Stp] = URL) then exit;
  while STP < URLStack.Count - 1 do begin
    URLStack.Delete(Stp);
    TargetStack.Delete(Stp);
  end;
  URLStack.Add(URL);
  TargetStack.Add(Target);
  Stp := URLStack.Count - 1;
end;

procedure TIpHtmlCustomScanner.EnumDocuments(Enumerator: TIpHtmlEnumerator);
begin
  if MasterFrame <> nil then
    MasterFrame.EnumDocuments(Enumerator);
end;

function TIpHtmlCustomScanner.IsURLHtml(const URL: string): Boolean;
var
  ResourceType: string;
begin
  Result := (FDataProvider <> nil)
  and FDataProvider.DoCheckURL(URL, ResourceType)
  and (CompareText(ResourceType, 'text/html') = 0);
end;

procedure TIpHtmlCustomScanner.Stop;
begin
  if assigned(MasterFrame) then
    MasterFrame.Stop;
end;

{Begin !!.14}
function TIpHtmlCustomScanner.GetVersion : string;
begin
  Result := IpShortVersion;
end;

procedure TIpHtmlCustomScanner.SetVersion(const Value : string);
begin
  { Intentionally empty }
end;
{End !!.14}

{$IFDEF IP_LAZARUS}
function LazFlatSB_GetScrollInfo(hWnd: HWND; BarFlag: Integer;
  var ScrollInfo: TScrollInfo): BOOL; stdcall;
begin
  Result:=LCLIntf.GetScrollInfo(HWnd,BarFlag,ScrollInfo);
end;
  
function LazFlatSB_GetScrollPos(hWnd: HWND; nBar: Integer): Integer; stdcall;
begin
  Result:=LCLIntf.GetScrollPos(HWnd,nBar);
end;

function LazFlatSB_SetScrollPos(hWnd: HWND; nBar, nPos: Integer;
  bRedraw: BOOL): Integer; stdcall;
begin
  Result:=LCLIntf.SetScrollPos(HWnd,nBar,nPos,bRedraw);
end;

function LazFlatSB_SetScrollProp(p1: HWND; index: Integer; newValue: Integer;
  p4: Bool): Bool; stdcall;
begin
  // ToDo
  Result:=true;
end;

function LazFlatSB_SetScrollInfo(hWnd: HWND; BarFlag: Integer;
  const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;
begin
  Result:=LCLIntf.SetScrollInfo(HWnd,BarFlag,ScrollInfo,Redraw);
end;
{$ENDIF}


procedure InitScrollProcs;
{$IFNDEF IP_LAZARUS}
var
  ComCtl32: THandle;
{$ENDIF}
begin
  {$IFDEF IP_LAZARUS}
  @FlatSB_GetScrollInfo := @LazFlatSB_GetScrollInfo;
  @FlatSB_GetScrollPos :=  @LazFlatSB_GetScrollPos;
  @FlatSB_SetScrollPos :=  @LazFlatSB_SetScrollPos;
  @FlatSB_SetScrollProp := @LazFlatSB_SetScrollProp;
  @FlatSB_SetScrollInfo := @LazFlatSB_SetScrollInfo;
  {$ELSE}
  ComCtl32 := GetModuleHandle('comctl32.dll');
  @FlatSB_GetScrollInfo := GetProcAddress(ComCtl32, 'FlatSB_GetScrollInfo');
  @FlatSB_GetScrollPos := GetProcAddress(ComCtl32, 'FlatSB_GetScrollPos');
  @FlatSB_SetScrollPos := GetProcAddress(ComCtl32, 'FlatSB_SetScrollPos');
  @FlatSB_SetScrollProp := GetProcAddress(ComCtl32, 'FlatSB_SetScrollProp');
  @FlatSB_SetScrollInfo := GetProcAddress(ComCtl32, 'FlatSB_SetScrollInfo');
  {$ENDIF}
end;



{ TIntArr }

destructor TIntArr.Destroy;
begin
  inherited;
  Freemem(InternalIntArr);
end;

function TIntArr.GetValue(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= IntArrSize) then
    Result := 0
  else
    Result := InternalIntArr^[Index];
end;

procedure TIntArr.SetValue(Index, Value: Integer);
var
  {$IFNDEF IP_LAZARUS}
  Tmp: PInternalIntArr;
  {$ENDIF}
  NewSize: Integer;
  p: ^Integer;
begin
  if Index >= 0 then begin
    if Index >= IntArrSize then begin
      NewSize := IntArrSize;
      repeat
        inc(NewSize, TINTARRGROWFACTOR);
      until Index < NewSize;
      {$IFDEF IP_LAZARUS code below does not check if InternalIntArr<>nil}
      ReallocMem(InternalIntArr,NewSize * sizeof(PtrInt));
      p := pointer(InternalIntArr);
      inc(p, IntArrSize);
      fillchar(p^, (NewSize - IntArrSize)*sizeOf(PtrInt), 0);
      IntArrSize := NewSize;
      {$ELSE}
      Tmp := AllocMem(NewSize * sizeof(Integer));
      move(InternalIntArr^, Tmp^, IntArrSize * sizeof(Integer));
      IntArrSize := NewSize;                                           {!!.12}
      {inc(IntArrSize, NewSize);}                                      {Deleted !!.12}
      Freemem(InternalIntArr);
      InternalIntArr := Tmp;
      {$ENDIF}
    end;
    InternalIntArr^[Index] := Value;
  end;
end;

{ TRectArr }

destructor TRectArr.Destroy;
begin
  inherited;
  Freemem(InternalRectArr);
end;

{
function TRectArr.GetRect(Index: Integer): PRect;
begin
  Assert(Self <> nil);
  if (Index < 0) then begin
    Result := nil;
    exit;
  end;
  if (Index >= IntArrSize) then
    SetValue(Index, NullRect);
  Result := @InternalRectArr^[Index];
end;
}

function TRectArr.GetValue(Index: Integer): PRect;
begin
  Assert(Self <> nil);
  if (Index < 0) or (Index >= IntArrSize) then
    Result := nil
  else
    Result := InternalRectArr^[Index];
end;

procedure TRectArr.SetValue(Index: Integer; Value: PRect);
var
  {$IFNDEF IP_LAZARUS}
  Tmp: PInternalRectArr;
  {$ELSE}
  P: Pointer;
  {$ENDIF}
  NewSize: Integer;
begin
  Assert(Self <> nil);
  if Index >= 0 then begin
    if Index >= IntArrSize then begin
      NewSize := IntArrSize;
      repeat
        inc(NewSize, TINTARRGROWFACTOR);
      until Index < NewSize;
      {$IFDEF IP_LAZARUS code below does not check if InternalIntArr<>nil and set buggy IntArrSize}
      ReallocMem(InternalRectArr,NewSize * sizeof(PtrInt));
      P := pointer(InternalRectArr);
      inc(P, IntArrSize);
      fillchar(p^, (NewSize - IntArrSize)*sizeOf(PtrInt), 0);
      IntArrSize:=NewSize;
      {$ELSE}
      Tmp := AllocMem(NewSize * sizeof(Integer));
      move(InternalRectArr^, Tmp^, IntArrSize * sizeof(Integer));
      inc(IntArrSize, NewSize);
      Freemem(InternalRectArr);
      InternalRectArr := Tmp;
      {$ENDIF}
    end;
    InternalRectArr^[Index] := Value;
  end;
end;

{ TRectRectArr }

procedure TRectRectArr.Delete(Index: Integer);
var
  i: Integer;
begin
  if (Index >= 0) and (Index < IntArrSize) then begin
    Value[Index].Free;
    for i := 1 to IntArrSize - 1 do
      InternalRectRectArr[i-1] := InternalRectRectArr[i];
    InternalRectRectArr[IntArrSize - 1] := nil;
  end;
end;

destructor TRectRectArr.Destroy;
var
  i: Integer;
begin
  inherited;
  for i := 0 to IntArrSize - 1 do
    Delete(i);
  if InternalRectRectArr <> nil then
    Freemem(InternalRectRectArr);
end;

function TRectRectArr.GetValue(Index: Integer): TRectArr;
var
  {$IFNDEF IP_LAZARUS}
  Tmp: PInternalRectRectArr;
  {$ELSE}
  P: ^Pointer;
  {$ENDIF}
  NewSize: Integer;
begin
  if Index >= 0 then begin
    if Index >= IntArrSize then begin
      NewSize := IntArrSize;
      repeat
        inc(NewSize, TINTARRGROWFACTOR);
      until Index < NewSize;
      {$IFDEF IP_LAZARUS code below does not check if InternalIntArr<>nil and set buggy IntArrSize}
      ReallocMem(InternalRectRectArr,NewSize * sizeof(PtrInt));
      p := pointer(InternalRectRectArr);
      inc(p, IntArrSize);
      fillchar(p^, (NewSize - IntArrSize)*sizeOf(PtrInt), 0);
      IntArrSize:=NewSize;
      {$ELSE}
      Tmp := AllocMem(NewSize * sizeof(Integer));
      move(InternalRectRectArr^, Tmp^, IntArrSize * sizeof(Integer));
      inc(IntArrSize, NewSize);
      Freemem(InternalRectRectArr);
      InternalRectRectArr := Tmp;
      {$ENDIF}
    end;
    Result := InternalRectRectArr^[Index];
    if Result = nil then begin
      Result := TRectArr.Create;
      InternalRectRectArr^[Index] := Result;
    end;
  end else
    Result := nil;
end;

{ TIpHtmlPrintSettings }

constructor TIpHtmlPrintSettings.Create;
begin
  inherited;
  FMarginLeft := DEFAULT_PRINTMARGIN;
  FMarginTop := DEFAULT_PRINTMARGIN;
  FMarginRight := DEFAULT_PRINTMARGIN;
  FMarginBottom := DEFAULT_PRINTMARGIN;
end;

destructor TIpHtmlPrintSettings.Destroy;
begin
  inherited;
end;

initialization
{$IFDEF IP_LAZARUS}
{$I iphtml.lrs}
{$ENDIF}
  InitScrollProcs;
end.

