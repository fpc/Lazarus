{Version 7.5}
{*********************************************************}
{*                     LITEPARS.PAS                      *}
{*              Copyright (c) 1995-2002 by               *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i LiteCons.inc}

{             The Parser
This module contains the parser which reads thru the document.  It divides it
into sections storing the pertinent information in Section objects.  The
document itself is then a TList of section objects.  See the LiteSubs unit for
the definition of the section objects.

Key Variables:

  Sy:
      An enumerated type which indicates what the current token is.  For
      example, a value of TextSy would indicate a hunk of text, PSy that a <P>
      tag was encountered, etc.
  LCh:
      The next character in the stream to be analyzed.  In mixed case.
  Ch:
      The same character in upper case.
  LCToken:
      A string which is associated with the current token.  If Sy is TextSy,
      then LCToken contains the text.
  Attributes:
      A list of TAttribute's for tokens such as <img>, <a>, which have
      attributes.
  Section:
      The current section being built.
  SectionList:
      The list of sections which form the document.  When in a Table,
      SectionList will contain the list that makes up the current cell.

Key Routines:

  GetCh:
      Gets the next character from the stream.  Fills Ch and LCh.  Skips
      comments.
  Next:
      Gets the next token.  Fills Sy, LCToken, Attributes.  Calls GetCh so the
      next character after the present token is available.  Each part of the
      parser is responsible for calling Next after it does its thing.
}

unit LitePars;

interface
uses
  {$IFDEF HL_LAZARUS}
  Classes, SysUtils, LCLType, Messages, GraphType, Graphics, Controls,
  Dialogs, StdCtrls, LiteUn2, LiteSubs, LiteSbs1;
  {$ELSE}
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Dialogs, StdCtrls, LiteUn2, LiteSubs, LiteSbs1;
  {$ENDIF}

const
  MaxStack = 25;
  FontConv: array[1..7] of integer = (8,10,12,14,18,24,36);
  PreFontConv:  array[1..7] of integer = (7,8,10,12,15,20,30);

type
  LoadStyleType = (lsPrimary, lsInclude);
  TIncludeType = procedure(Sender: TObject; const Command: string;
                    Params: TStrings; var IString: string) of Object;  
  TSoundType = procedure(Sender: TObject; const SRC: string; Loop: integer;
                    Terminate: boolean) of Object;
  TMetaType = procedure(Sender: TObject; const HttpEq, Name, Content: string) of Object;

  SymbSet = Set of Symb;

  TLastChar = (lcOther, lcCR, lcLF);

  ThlParser = Class
  private
    Sy : Symb;
    PreFormat: boolean;    {set when doing preformat <pre> text}
    Justify: JustifyType;
    BaseFontSize: integer;
    InScript: boolean;     {when in a <SCRIPT>}
    NoPSpace: boolean;      {start a new cell (to avoid paragraph space on <p>}
    TagIndex: integer;
    Section : TSection;
    SectionList: TCell;
    MasterList: TSectionList;
    NameList: TStringList;  {a list of the <a Name= > attributes along with the
                             section where found}
    FontStack: array[1..MaxStack] of TMyFont; {handy way of keeping track of past
                                             fonts}
    StackIndex: integer;
    CurrentURLTarget: TURLTarget;
    InHref: boolean;
    Attributes : TAttributeList;

    LCh, Ch: Char;
    SIndex: integer;
    LastChar: TLastChar;
    Value : integer;
    LCToken : TokenObj;
    LoadStyle: LoadStyleType;
    Buffer, BuffEnd: PChar;

    IBuff, IBuffEnd: PChar;
    SIBuff: string;
    IncludeEvent: TIncludeType;
    CallingObject: TObject;
    SaveLoadStyle: LoadStyleType;
    SoundEvent: TSoundType;
    MetaEvent: TMetaType;
    NoBreak: boolean;       {set when in <NoBr>}
    ALoop: integer;
    ATName: string;
    HttpEq, Content: string;
    InclSL: TStringList;
    InclS: string;

    procedure SkipWhiteSpace;
    procedure GetCh;
    function GetAttribute(var Sym: Symb;
      var St, S: {$IFDEF HL_LAZARUS}HLString{$ELSE}OpenString{$ENDIF};
      var Val: Integer): boolean;
    function GetID(
      var S: {$IFDEF HL_LAZARUS}HLString{$ELSE}OpenString{$ENDIF}): boolean;
    function GetQuotedStr(
      var S: {$IFDEF HL_LAZARUS}HLString{$ELSE}OpenString{$ENDIF};
      var {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF}: Integer): boolean;
    procedure GetSomething(
      var S: {$IFDEF HL_LAZARUS}HLString{$ELSE}OpenString{$ENDIF});
    function GetTag: boolean;
    function GetValue(var S: string;
      var {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF}: Integer): boolean;
    function IsOddChar: boolean;
    function IsText: boolean;
    procedure Next;
    procedure DoAEnd;
    procedure DoBase;
    procedure DoBody(Level: integer; const TermSet: SymbSet);
    procedure DoCommonSy(Lev: integer);
    procedure DoLists(Level: integer; Sym: Symb; const TermSet: SymbSet);
    procedure DoMap;
    procedure DoMeta(Sender: TObject);
    procedure DoScript(Ascript: TScriptEvent);
    procedure DoSound;
    procedure DoTable(ALevel: integer);
    procedure DoText;
    procedure DoTextArea(TxtArea: TTextAreaFormControlObj);
    procedure DoTitle;
    procedure GetOptions(Select: TListBoxFormControlObj);
    procedure ParseInit(ASectionList: TList; ANameList: TStringList;
      AIncludeEvent: TIncludeType);
    procedure PopFont;
    function FindAlignment: JustifyType;
    function PushNewFont: boolean;
    function TranslateCharset(DefCharset: TFontCharset;
      const {$IFDEF HL_LAZARUS}NewContent{$ELSE}Content{$ENDIF}: string): TFontCharset;
    procedure SignalSuspend;
    procedure DoSoundEvent;
    procedure DoMetaEvent;
    procedure DoIncludeEvent;
  public
    Title: string;
    Base: string;
    BaseTarget: string;
    AllowSuspend: boolean;
    ParseThread: {$IFDEF NoThreads}TObject{$ELSE}TThread{$ENDIF};
    CurrentStyle: TFontStyles;  {as set by <b>, <i>, etc.}
    CurrentSScript: SubSuperType;
    CurrentForm: ThtmlForm;

    constructor Create;
    destructor Destroy; override;
    procedure HtmlParseTextString(ASectionList: TList;
      ANameList: TStringList);
    procedure HtmlParseString(ASectionList: TList; ANameList: TStringList;
      AIncludeEvent: TIncludeType; ASoundEvent: TSoundType;
      AMetaEvent: TMetaType);
  end;

function GetColor(S: String; Var Value: TColor): boolean;


implementation

uses
  htmllite, LiteReadThd;

Const
  Tab = #9;
  EofChar = ^Z;

type
  SymString = string[12];

Const
  MaxRes = 66;
  MaxEndRes = 49;
  ResWords : array[1..MaxRes] of SymString =
     ('HTML', 'TITLE', 'BODY', 'HEAD', 'B', 'I', 'H', 'EM', 'STRONG',
      'U', 'CITE', 'VAR', 'TT', 'CODE', 'KBD', 'SAMP', 'OL', 'UL', 'DIR',
      'MENU', 'DL',
      'A', 'ADDRESS', 'BLOCKQUOTE', 'PRE', 'CENTER', 'TABLE', 'TD', 'TH',
      'CAPTION', 'FORM', 'TEXTAREA', 'SELECT', 'OPTION', 'FONT', 'SUB', 'SUP',
      'BIG', 'SMALL', 'P', 'MAP', 'FRAMESET', 'NOFRAMES', 'SCRIPT', 'DIV',
      'S', 'STRIKE', 'TR', 'NOBR',

      'LI', 'BR', 'HR', 'DD', 'DT', 'IMG', 'BASE', 'BUTTON','INPUT',
      'SELECTED', 'BASEFONT', 'AREA', 'FRAME', 'BGSOUND', 'WRAP',
      'META', 'WBR');

  ResSy : array[1..MaxRes] of Symb =
     (htmlSy, TitleSy, BodySy, HeadSy, BSy, ISy, HeadingSy, EmSy, StrongSy,
      USy, CiteSy, VarSy, TTSy, CodeSy, KbdSy, SampSy, OLSy, ULSy, DirSy,
      MenuSy, DLSy, ASy, AddressSy, BlockQuoteSy, PreSy, CenterSy,TableSy,
      TDsy, THSy, CaptionSy, FormSy, TextAreaSy,  SelectSy, OptionSy, FontSy,
      SubSy, SupSy, BigSy, SmallSy, PSy, MapSy, FrameSetSy, NoFramesSy,
      ScriptSy, DivSy, SSy, StrikeSy, TRSy, NoBrSy,

      LISy, BRSy, HRSy, DDSy, DTSy, ImageSy, BaseSy, ButtonSy,
      InputSy, SelectedSy, BaseFontSy, AreaSy, FrameSy, BgSoundSy,
      WrapSy, MetaSy, WbrSy);

  {keep these in order with those above}
  EndResSy : array[1..MaxEndRes] of Symb =
     (HtmlEndSy, TitleEndSy, BodyEndSy, HeadEndSy, BEndSy, IEndSy, HeadingEndSy,
      EmEndSy, StrongEndSy, UEndSy, CiteEndSy, VarEndSy, TTEndSy, CodeEndSy,
      KbdEndSy, SampEndSy,
      OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy, AEndSy, AddressEndSy,
      BlockQuoteEndSy, PreEndSy, CenterEndSy, TableEndSy, TDEndSy, THEndSy,
      CaptionEndSy, FormEndSy, TextAreaEndSy, SelectEndSy, OptionEndSy, FontEndSy,
      SubEndSy, SupEndSy, BigEndSy, SmallEndSy, PEndSy, MapEndSy, FrameSetEndSy,
      NoFramesEndSy, ScriptEndSy, DivEndSy, SEndSy, StrikeEndSy, TREndSy, NoBrEndSy);

  TextBuffSize = 2000;

Type
  EParseError = class(Exception);

procedure ThlParser.SignalSuspend;
begin
(CallingObject as ThtmlLite).SignalSuspend;
end;

procedure ThlParser.DoIncludeEvent;
begin
IncludeEvent(CallingObject, InclS, InclSL, SIBuff);
end;

{-------------GetCh}
PROCEDURE ThlParser.GetCh;
{Return next char in Lch, its uppercase value in Ch.  Ignore comments}
var
  Comment, Done: boolean;

  function ReadChar: char;
  begin
  case LoadStyle of
    lsPrimary:
       begin
       if Buffer < BuffEnd then
         begin
         Result := Buffer^;
         Inc(Buffer);
         Inc(SIndex);
         end
       else if AllowSuspend then
         begin
         TParseThread(ParseThread).Buffer := Buffer;
         TParseThread(ParseThread).Synchronize(
           {$IFDEF HL_LAZARUS}@{$ENDIF}SignalSuspend);
         TParseThread(ParseThread).Suspend;  {wait for more}
         Buffer := TParseThread(ParseThread).Buffer;
         BuffEnd := TParseThread(ParseThread).BuffEnd;
         if Buffer < BuffEnd then
           begin
           Result := Buffer^;
           Inc(Buffer);
           Inc(SIndex);
           end
         else
           Result := EOFChar;
         end
       else
         Result := EOFChar;
       end;

    lsInclude:
       if IBuff < IBuffEnd then
         begin
         Result := IBuff^;
         Inc(IBuff);
         end
       else
         begin
         LoadStyle := SaveLoadStyle;
         Result := ReadChar;
         end;
    else Result := #0;       {to prevent warning msg}
    end;
  end;

  function Peek: char;  {take a look at the next char}
  begin
  case LoadStyle of
    lsPrimary:
       begin
       if Buffer < BuffEnd then
         Result := Buffer^
       else
         Result := EOFChar;
       end;

    lsInclude:
       if IBuff < IBuffEnd then
         Result := IBuff^
       else
         begin
         LoadStyle := SaveLoadStyle;
         Result := Peek;
         end;
    else Result := #0;       {to prevent warning msg}
    end;
  end;

  procedure GetchBasic; {read a character}   
  begin
  LCh := ReadChar;
  case LCh of    {skip a ^J after a ^M or a ^M after a ^J}
    ^M: if LastChar = lcLF then
          LCh := ReadChar;
    ^J: if LastChar = lcCR then
          LCh := ReadChar;
    end;
  case LCh of
    ^M: LastChar := lcCR;
    ^J: begin
        LastChar := lcLF;
        LCh := ^M;
        end;
    else
        begin
        LastChar := lcOther;
        if LCh = Tab then LCh := ' ';
        end;
    end;
  Ch := UpCase(LCh);
  if (LCh = EofChar) and Comment then
    Raise EParseError.Create('Open Comment at End of HTML File');
  end;

  procedure DoDashDash;   {do the comment after a <!-- }
  begin
  repeat
    while Ch <> '-' do GetChBasic; {get first '-'}
    GetChBasic;
    if Ch = '-' then  {second '-'}
      begin
      while Ch = '-' do GetChBasic;  {any number of '-'}
      while (Ch = ' ') or (Ch = ^M) do GetChBasic;  {eat white space}
      Done := Ch = '>';
      end
    else Done := False;
  until Done;
  end;

  procedure ReadToGT;    {read to the next '>' }
  begin
  while Ch <> '>' do
    GetChBasic;
  end;

  procedure DoInclude;
  var
    SymStr, AttrStr: String;
    L: integer;
    Sym: Symb;
  begin
  InclS := '';
  GetChBasic;
  while Ch in ['A'..'Z'] do
    begin
    InclS := InclS + LCh;
    GetChBasic;
    end;
  InclSL := TStringList.Create;
  while GetAttribute(Sym, SymStr, AttrStr, L) do
    InclSL.Add(SymStr+'="'+AttrStr+'"');
  DoDashDash;
  SIBuff := '';
  TParseThread(ParseThread).Synchronize(
    {$IFDEF HL_LAZARUS}@{$ENDIF}DoIncludeEvent);
  if Length(SIBuff) > 0 then
    begin
    SaveLoadStyle := LoadStyle;
    LoadStyle := lsInclude;
    IBuff := PAnsiChar(SIBuff);
    IBuffEnd := IBuff+Length(SIBuff);
    end;
  end;

begin  {Getch}
repeat    {in case a comment immediately follows another comment}
   {comments may be either '<! stuff >' or '<!-- stuff -->'  }
  Comment := False;
  GetchBasic;
  if (Ch = '<') and not InScript then
    begin
    if Peek = '!' then   
      begin
      GetChBasic;  
      Comment:=True;
      GetChBasic;
      if Ch = '-' then
        begin
        GetChBasic;
        if Ch = '-' then
          begin
          GetChBasic;
          if Assigned(IncludeEvent) and (Ch = '#') and (LoadStyle <> lsInclude) then
            DoInclude
          else
            DoDashDash;  {a <!-- comment}
          end
        else ReadToGT;
        end
      else ReadToGT;
      end;
    end;
until not Comment;
end;

{-------------SkipWhiteSpace}
procedure ThlParser.SkipWhiteSpace;
begin
while (LCh in [' ', Tab, ^M]) do
  GetCh;
end;

{----------------GetValue}
function ThlParser.GetValue(var S: string;
  var {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF}: integer): boolean;
{read a numeric}
var
  Code: integer;
begin
Result := Ch in ['-', '+', '0'..'9'];
if not Result then Exit;
{$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF} := 0;
if Ch in ['-', '+'] then
  begin
  S := Ch;
  GetCh;
  end
else S := '';
while not (Ch in [' ', Tab, ^M, '>', '%', EofChar]) do
  begin
  S := S + Ch;
  GetCh;
  end;
SkipWhiteSpace;
Val(S, {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF}, Code);
if Code <> 0 then
  {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF} := 0;
if LCh = '%' then
  begin
  S := S + '%';
  GetCh;
  end;
end;

{----------------IsOddChar}
function ThlParser.IsOddChar : boolean;
const
  Lim = 8;  
  MaxChars = Lim+96;
  OddCharStr : array[1..MaxChars] of string[6] =
    ('amp', 'lt', 'gt', 'quot', 'rsquo', 'ndash', 'trade', 'hellip', {Lim special items} 
'nbsp',      {no-break space}
'iexcl',     {inverted exclamation mark}
'cent',      {cent sign}
'pound',     {pound sterling sign}
'curren',    {general currency sign}
'yen',       {yen sign}
'brvbar',    {broken (vertical) bar}
'sect',      {section sign}
'uml',       {umlaut (dieresis)}
'copy',      {copyright sign}
'ordf',      {ordinal indicator', feminine}
'laquo',     {angle quotation mark', left}
'not',       {not sign}
'shy',       {soft hyphen}
'reg',       {registered sign}
'macr',      {macron}
'deg',       {degree sign}
'plusmn',    {plus-or-minus sign}
'sup2',      {superscript two}
'sup3',      {superscript three}
'acute',     {acute accent}
'micro',     {micro sign}
'para',      {pilcrow (paragraph sign)}
'middot',    {middle dot}
'cedil',     {cedilla}
'sup1',      {superscript one}
'ordm',      {ordinal indicator', masculine}
'raquo',     {angle quotation mark', right}
'frac14',    {fraction one-quarter}
'frac12',    {fraction one-half}
'frac34',    {fraction three-quarters}
'iquest',    {inverted question mark}

'Agrave',    {Capital A, grave accent}
'Aacute',    {Capital A, acute accent}
'Acirc',     {Capital A, circumflex accent}
'Atilde',    {Capital A, tilde}
'Auml',      {Capital A, dieresis or umlaut mark}
'Aring',     {Capital A, ring}
'AElig',     {Capital AE dipthong (ligature)}
'Ccedil',    {Capital C, cedilla}
'Egrave',    {Capital E, grave accent}
'Eacute',    {Capital E, acute accent}
'Ecirc',     {Capital E, circumflex accent}
'Euml',      {Capital E, dieresis or umlaut mark}
'Igrave',    {Capital I, grave accent}
'Iacute',    {Capital I, acute accent}
'Icirc',     {Capital I, circumflex accent}
'Iuml',      {Capital I, dieresis or umlaut mark}
'ETH',       {Capital Eth, Icelandic}
'Ntilde',    {Capital N, tilde}
'Ograve',    {Capital O, grave accent}
'Oacute',    {Capital O, acute accent}
'Ocirc',     {Capital O, circumflex accent}
'Otilde',    {Capital O, tilde}
'Ouml',      {Capital O, dieresis or umlaut mark}
'times',          {missing}
'Oslash',    {Capital O, slash}
'Ugrave',    {Capital U, grave accent}
'Uacute',    {Capital U, acute accent}
'Ucirc',     {Capital U, circumflex accent}
'Uuml',      {Capital U, dieresis or umlaut mark}
'Yacute',    {Capital Y, acute accent}
'THORN',     {Capital THORN, Icelandic}
'szlig',     {Small sharp s, German (sz ligature)}
'agrave',    {Small a, grave accent}
'aacute',    {Small a, acute accent}
'acirc',     {Small a, circumflex accent}
'atilde',    {Small a, tilde}
'auml',      {Small a, dieresis or umlaut mark}
'aring',     {Small a, ring}
'aelig',     {Small ae dipthong (ligature)}
'ccedil',    {Small c, cedilla}
'egrave',    {Small e, grave accent}
'eacute',    {Small e, acute accent}
'ecirc',     {Small e, circumflex accent}
'euml',      {Small e, dieresis or umlaut mark}
'igrave',    {Small i, grave accent}
'iacute',    {Small i, acute accent}
'icirc',     {Small i, circumflex accent}
'iuml',      {Small i, dieresis or umlaut mark}
'eth',       {Small eth, Icelandic}
'ntilde',    {Small n, tilde}
'ograve',    {Small o, grave accent}
'oacute',    {Small o, acute accent}
'ocirc',     {Small o, circumflex accent}
'otilde',    {Small o, tilde}
'ouml',      {Small o, dieresis or umlaut mark}
'divide',          {missing}
'oslash',    {Small o, slash}
'ugrave',    {Small u, grave accent}
'uacute',    {Small u, acute accent}
'ucirc',     {Small u, circumflex accent}
'uuml',      {Small u, dieresis or umlaut mark}
'yacute',    {Small y, acute accent}
'thorn',     {Small thorn, Icelandic}
'yuml');     {Small y, dieresis or umlaut mark}


  OddChar : array[1..Lim] of string[3] =
    ('&', '<', '>', '"', '''', '-', #153, '...');
Label 2;
var
  I, J, N: integer;
  S: string[8];
  Collect: TokenObj;   
  SaveIndex: integer;
  W: WideChar;

  procedure NextCh; 
  begin {collect chars and Indices in case of failure}
  Collect.AddChar(LCh, SIndex);
  GetCh;
  end;

begin
if Ch <> '&' then
  begin
  Result := False;
  Exit;
  end;
Result := True;
Sy := TextSy;
I := 0;
S := '';
SaveIndex := SIndex;  
Collect := TokenObj.Create;  
try
  NextCh;
  if Ch = '#' then
    begin   {look for char #}
    NextCh;
    while I <=5 do
      begin
      if not (Ch in ['0'..'9']) then
        begin
        if Ch = ';' then NextCh;
        if S <> '' then
          begin
          N := StrToInt(S);
          if (N > 255) then    
            begin     
            W := WideChar(N);
            S := WideCharLenToString(@W, 1);
            LCToken.AddChar(S[1], SaveIndex);
            GoTo 2;
            end
          else if (byte(N) in [9, 10, 32..255]) then
            begin
            if N = 9 then LCToken.AddChar(' ', SaveIndex)
            else LCToken.AddChar(chr(N), SaveIndex);
            GoTo 2;
            end;
          end;
        LCToken.Concat(Collect);
        GoTo 2;
        end;
      S := S+LCh;
      Inc(I);
      NextCh;
      end;
    LCToken.Concat(Collect);
    GoTo 2;
    end
  else while I <= 6 do
    begin
    if not (Ch in ['A'..'Z', '0'..'9']) then  {note: Ch is always upper case}
      begin
      if Ch = ';' then NextCh;
      for  J := 1 to MaxChars do
        if S = OddCharStr[J] then
          begin
          if J <= Lim then
            if S = 'hellip' then
              begin
              LCToken.S := '...';
              LCToken.I^[1] := SaveIndex;
              LCToken.I^[2] := SaveIndex+1;
              LCToken.I^[3] := SaveIndex+2;
              end
            else
              LCToken.AddChar(OddChar[J][1], SaveIndex)
          else LCToken.AddChar(chr(J - Lim + 159), SaveIndex);
          GoTo 2;
          end;
      LCToken.Concat(Collect);
      GoTo 2;
      end;
    S := S+LCh;
    Inc(I);
    NextCh;
    end;
  {too many chars, assume it's just text}
  LCToken.Concat(Collect);
2: ;
finally
  Collect.Free;
  end;
end;

{----------------GetQuotedStr}
function ThlParser.GetQuotedStr(
  var S: {$IFDEF HL_LAZARUS}HLString{$ELSE}OpenString{$ENDIF};
  var {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF}: integer): boolean;
{get a quoted string but strip the quotes, check to see if it is numerical}
var
  Term: char;
  S1: string;
  Code: integer;
  SaveSy: Symb;
begin
Result := False;
Term := Ch;
if (Term <> '"') and (Term <> '''') then Exit;
Result := True;
SaveSy := Sy;
GetCh;
while not (Ch in [Term, EofChar]) do
  begin
  if LCh <> ^M then
    begin
    if IsOddChar then  
      begin
      S := S + LCToken.S;
      LCToken.Clear;       
      end
    else
      begin
      S := S + LCh;
      GetCh;
      end;
    end
  else GetCh;   {pass ^M by}
  end;
if Ch = Term then GetCh;  {pass termination char}
S1 := Trim(S);
if Pos('%', S1) = Length(S1) then SetLength(S1, Length(S1)-1);
Val(S1, {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF}, Code);
if Code <> 0 then
  {$IFDEF HL_LAZARUS}NewValue{$ELSE}Value{$ENDIF} := 0;
Sy := SaveSy;
end;

{----------------GetSomething}
procedure ThlParser.GetSomething(
  var S: {$IFDEF HL_LAZARUS}HLString{$ELSE}OpenString{$ENDIF});

begin
while not (Ch in [' ', Tab, ^M, '>', EofChar]) do
  begin
  S := S+LCh;
  GetCh;
  end;
end;

{----------------GetID}
function ThlParser.GetID(
  var S: {$IFDEF HL_LAZARUS}HLString{$ELSE}OpenString{$ENDIF}): boolean;

begin
Result := False;
if not (Ch in ['A'..'Z']) then Exit;
while Ch in ['A'..'Z', '-'] do
  begin
  S := S+Ch;
  GetCh;
  end;
Result := True;
end;

{----------------GetAttribute}
function ThlParser.GetAttribute(var Sym: Symb;
  var St, S: {$IFDEF HL_LAZARUS}HLString{$ELSE}OpenString{$ENDIF};
  var Val: integer): boolean;

const
  MaxAttr = 63;
  Attrib : array[1..MaxAttr] of string[12] =
     ('HREF', 'NAME', 'SRC', 'ALT', 'ALIGN', 'TEXT', 'BGCOLOR', 'LINK',
      'BACKGROUND', 'COLSPAN', 'ROWSPAN', 'BORDER', 'CELLPADDING',
      'CELLSPACING', 'VALIGN', 'WIDTH', 'START', 'VALUE', 'TYPE',
      'CHECKBOX', 'RADIO', 'METHOD', 'ACTION', 'CHECKED', 'SIZE',
      'MAXLENGTH', 'COLS', 'ROWS', 'MULTIPLE', 'VALUE', 'SELECTED',
      'FACE', 'COLOR', 'TRANSP', 'CLEAR', 'ISMAP', 'BORDERCOLOR',
      'USEMAP', 'SHAPE', 'COORDS', 'NOHREF', 'HEIGHT', 'PLAIN', 'TARGET',
      'NORESIZE', 'SCROLLING', 'HSPACE', 'LANGUAGE', 'FRAMEBORDER',
      'MARGINWIDTH', 'MARGINHEIGHT', 'LOOP', 'ONCLICK', 'WRAP', 'NOSHADE',
      'HTTP-EQUIV', 'CONTENT', 'ENCTYPE', 'VLINK', 'OLINK', 'ACTIVE',
      'VSPACE', 'NOWRAP');   
  AttribSym: array[1..MaxAttr] of Symb =
     (HrefSy, NameSy, SrcSy, AltSy, AlignSy, TextSy, BGColorSy, LinkSy,
      BackgroundSy, ColSpanSy, RowSpanSy, BorderSy, CellPaddingSy,
      CellSpacingSy, VAlignSy, WidthSy, StartSy, ValueSy, TypeSy,
      CheckBoxSy, RadioSy, MethodSy, ActionSy, CheckedSy, SizeSy,
      MaxLengthSy, ColsSy, RowsSy, MultipleSy, ValueSy, SelectedSy,
      FaceSy, ColorSy, TranspSy, ClearSy, IsMapSy, BorderColorSy,
      UseMapSy, ShapeSy, CoordsSy, NoHrefSy, HeightSy, PlainSy, TargetSy,
      NoResizeSy, ScrollingSy, HSpaceSy, LanguageSy, FrameBorderSy,
      MarginWidthSy, MarginHeightSy, LoopSy, OnClickSy, WrapSy, NoShadeSy,
      HttpEqSy, ContentSy, EncTypeSy, VLinkSy, OLinkSy, ActiveSy,
      VSpaceSy, NoWrapSy);
var
  I: integer;
begin
Sym := OtherAttribute;
Result := False;
SkipWhiteSpace;
St := '';
if GetID(St) then
  begin
  for I := 1 to MaxAttr do
    if St = Attrib[I] then
      begin
      Sym := AttribSym[I];
      Break;
      end;
  end
else Exit;   {no ID}
SkipWhiteSpace;
S := '';
if Sym = BorderSy then Val := 1 else Val := 0;
Result := True;      {at least have an ID}
if Ch <> '=' then Exit;
GetCh;

SkipWhiteSpace;
if not GetQuotedStr(S, Val) then   {either it's a quoted string or a number}
if not GetValue(S, Val) then
  GetSomething(S);    {in case quotes left off string}
end;

{-------------GetTag}
function ThlParser.GetTag: boolean;  {Pick up a Tag or pass a single '<'}
Var
  Done, EndTag : Boolean;
  Compare: String[255];
  SymStr, AttrStr: string;
  I: Integer;
  L: integer;
  Save: integer;  
  Sym: Symb;
begin
if Ch <> '<' then
  begin
  Result := False;
  Exit;
  end
else Result := True;
Save := SIndex;   
TagIndex := SIndex;  
Compare := '';
GetCh;
if Ch = '/' then
  begin
  EndTag := True;
  GetCh;
  end
else if not (Ch in ['A'..'Z']) then
  begin     {an odd '<'}
  Sy := TextSy;
  LCToken.AddChar('<', Save);
  Exit;
  end
else
  EndTag := False;
Sy := CommandSy;
Done := False;
while not Done do
  case Ch of
    'A'..'Z' :
          begin
          if Length(Compare) < 255 then
            begin
            Inc(Compare[0]);
            Compare[Length(Compare)] := Ch;
            end;
          GetCh;
          end;
    else Done := True;
   end;
for I := 1 to MaxRes do
  if Compare = ResWords[I] then
    begin
    if not EndTag then
      Sy := ResSy[I]
    else
      if I <= MaxEndRes then
        Sy := EndResSy[I];   {else Sy  := CommandSy}
    Break;
    end;
SkipWhiteSpace;
Value := 0;
if ((Sy = HeadingSy) or (Sy = HeadingEndSy)) and (Ch in ['1'..'6']) then
  begin
  Value := ord(Ch)-ord('0');
  GetCh;
  end;

if Sy in [ImageSy, BaseSy, ASy, BodySy, TDSy, THSy, TRSy, TableSy,
      CaptionSy, OLSy, ULSy, InputSy, FormSy, TextAreaSy, SelectSy, OptionSy,
      PSy, FontSy, BaseFontSy, BRSy, HeadingSy, MapSy, AreaSy,
      HRSy, FrameSy, FrameSetSy, NoResizeSy, ScrollingSy, ScriptSy, DivSy,
      BgSoundSy, WrapSy, MetaSy ] then
  Attributes.Clear;
while GetAttribute(Sym, SymStr, AttrStr, L) do
  if Sy in [ImageSy, BaseSy, ASy, BodySy, TDSy, THSy, TRSy, TableSy,
     CaptionSy, OLSy, ULSy, InputSy, FormSy, TextAreaSy, SelectSy, OptionSy,
     PSy, FontSy, BaseFontSy, BRSy, HeadingSy, MapSy, AreaSy,
     HRSy, FrameSy, FrameSetSy, NoResizeSy, ScrollingSy, ScriptSy, DivSy,
     BgSoundSy, WrapSy, MetaSy] then
    Attributes.Add(TAttribute.Create(Sym, L, AttrStr));
       {else ignore (but pass over) other attributes}

while (Ch <> '>') and (Ch <> EofChar) do GetCh;
GetCh;
end;

{----------------IsText}
function ThlParser.IsText : boolean;
begin
LCToken.Clear;   
while (Length(LCToken.S) < 100) and    
      (LCh in [^M, ' '..'%', ''''..';', '=', '?'..#255, '>']) do
  if not PreFormat and ((LCh = ' ') or (LCh = ^M)) then
    begin
    if NoBreak then
      LCToken.AddChar(#5, SIndex)  {#5 is NoBreak space representation} 
    else 
      LCToken.AddChar(' ', SIndex);   {^M becomes space}
    SkipWhiteSpace;      {eliminate multiple spaces}
    end
  else
    begin
    LCToken.AddChar(LCh, SIndex);  
    GetCh;
    end;
if Length(LCToken.S) > 0 then
  begin
  Sy := TextSy;
  IsText := True;
  end
else IsText := False;
end;

{-----------Next}
PROCEDURE ThlParser.Next;
  {Get the next token}
begin  {already have fresh character loaded here}
LCToken.Clear;  
if LCh = EofChar then Sy := EofSy
else if GetTag then     {handles '<'}  
else if IsOddChar then
else if IsText then
else
  begin
  Sy := OtherChar;
  LCToken.AddChar(LCh, SIndex);  
  GetCh;
  end;
if NoPSpace and (Sy in [TextSy, ImageSy, TableSy, InputSy, SelectSy, TextAreaSy,
     BrSy, HeadingSy, HRSy]) then NoPSpace := False;
end;

function ThlParser.PushNewFont: boolean;
{add a font to the font stack identical to the last one}
begin
if StackIndex < MaxStack then
  begin
  Inc(StackIndex);
  FontStack[StackIndex] := TMyFont.Create;
  FontStack[StackIndex].Assign(FontStack[StackIndex-1]);
  Result := True;
  end
else Result := False;
end;

procedure ThlParser.PopFont;
{pop and free a font from the font stack}
begin
if StackIndex > 1 then
  begin
  FontStack[StackIndex].Free;
  Dec(StackIndex);
  end;
end;

procedure ThlParser.DoTextArea(TxtArea: TTextAreaFormControlObj);
{read and save the text for a TextArea form control}
var
  S: string;
  Token: string;  

  procedure Next1;
    {Special Next routine to get the next token}
    procedure GetTag1;  {simplified Pick up a Tag routine}
    begin
    Token := '<';
    GetCh;
    Sy := CommandSy;
    while not (LCh in [' ', ^M, Tab, '>']) do
      begin
      Token := Token + LCh;
      GetCh;
      end;
    if CompareText(Token, '</textarea') = 0 then
      Sy := TextAreaEndSy
    else Sy := CommandSy;    {anything else}
    end;

    function IsText1 : boolean;
    begin
    while (Length(Token) < 100) and  not (LCh in [^M, '<', '&', EofChar]) do
      begin
      Token := Token+LCh;
      GetCh;
      end;
    if Length(Token) > 0 then
      begin
      Sy := TextSy;
      IsText1 := True;
      end
    else IsText1 := False;
    end;

  begin  {already have fresh character loaded here}
  Token := '';
  LCToken.Clear;     
  if LCh = EofChar then Sy := EofSy
  else if LCh = ^M then
    begin
    Sy := EolSy;
    GetCh;
    end
  else if LCh = '<' then
     begin GetTag1;  Exit;  end
  else if IsOddChar then
    Token := LCToken.S
  else if IsText1 then
  else
    begin
    Sy := OtherChar;
    Token := LCh;
    GetCh;
    end;
  end;

begin
Next1;
S := '';
while (Sy <> TextAreaEndSy) and (Sy <> EofSy) do
  begin
  case Sy of
    TextSy: S := S + Token;
    EolSy:
      begin
      TxtArea.AddStr(S);
      S := '';
      end;
    else
      S := S + Token;
    end;
  Next1;
  end;
while not (LCh in ['>', EofChar]) do
  GetCh; {remove chars to and past '>'}
GetCh;
if S <> '' then TxtArea.AddStr(S);
end;

function ThlParser.FindAlignment: JustifyType;  {pick up Align= attribute}
var
  UpName: string[10];
  T: TAttribute;
begin
Result := Justify;
if Sy = PEndSy then Exit;
if Attributes.Find(AlignSy, T) then
  begin
  UpName := Lowercase(T.Name);
  if UpName = 'left' then Result := Left
  else if UpName = 'center' then Result := Centered
  else if UpName = 'right' then Result := Right;
  end;
end;

const
  TableTermSet = [TableEndSy, TDSy, TRSy, TREndSy, THSy, THEndSy, TDEndSy,
                  CaptionSy, CaptionEndSy];

procedure ThlParser.DoAEnd;  {do the </a>}
begin
if InHref then   {see if we're in an href}
  begin
  CurrentUrlTarget.Clear;
  InHref := False;
  PopFont;   {the blue stuff}
  end;
if Assigned(Section) then
  Section.HRef(AEndSy, MasterList, Nil, FontStack[StackIndex]);
end;

{----------------DoTable}
procedure ThlParser.DoTable(ALevel: integer);
var
  Table: ThtmlTable;
  SaveSectionList, JunkSaveSectionList: TCell;
  SaveStyle: TFontStyles;
  SaveNoBreak: boolean;
  SaveJustify, RowJustify, TmpJustify: JustifyType;
  RowVAlign, VAlign: AlignmentType;
  Row: TCellList;
  CellObj: TCellObj;
  T: TAttribute;
  SaveIndex: integer;

  function GetJustify: JustifyType;
  var
    S: string[9];
    T: TAttribute;
  begin
  Result := NoJustify;
  if Attributes.Find(AlignSy, T) then
    begin
    S := LowerCase(T.Name);
    if S = 'left' then Result := Left
    else if S = 'center' then Result := Centered
    else if S = 'right' then Result := Right;
    end;
  end;

  function GetVAlign(Default: AlignmentType): AlignmentType;
  var
    S: string[9];
    T: TAttribute;
  begin
  Result := Default;
  if Attributes.Find(VAlignSy, T) then
    begin
    S := LowerCase(T.Name);
    if (S = 'top') or (S = 'baseline') then Result := ATop 
    else if S = 'middle' then Result := AMiddle
    else if S = 'bottom' then Result := ABottom;
    end;
  end;

  procedure AddSection;
  begin
  if Assigned(SectionList) then
    begin
    SectionList.Add(Section);
    Section := Nil;
    if CellObj.Cell = SectionList then
      Row.Add(CellObj)
    else SectionList.Free;   {won't happen}
    SectionList := Nil;
    end;
  end;

begin
if InHref then DoAEnd;    {terminate <a>}
SaveIndex := StackIndex;
SectionList.Add(Section);
Section := Nil;
SaveSectionList := SectionList;
SaveJustify := Justify;
SaveStyle := CurrentStyle;
SaveNoBreak := NoBreak;   
SectionList := Nil;
Table := ThtmlTable.Create(MasterList, Attributes, Justify, SaveSectionList, ALevel);
Row := Nil;
RowJustify := NoJustify;
RowVAlign := AMiddle;  
Next;
while (Sy <> TableEndSy) and (Sy <> EofSy) do
  case Sy of
    TDSy, THSy:
      Begin
      if InHref then DoAEnd;
      while StackIndex > SaveIndex do
        PopFont;          {terminate any <font> introduced in table}
      CurrentStyle := SaveStyle;
      if Attributes.Find(NoWrapSy, T) and not Attributes.Find(WidthSy, T) then   
        NoBreak := True
      else NoBreak := False;     
      if not Assigned(Row) then   {in case <tr> is missing}
        begin
        Row := TCellList.Create;
        RowJustify := NoJustify;
        RowVAlign := AMiddle;   
        end
      else AddSection;
      if Sy = THSy then
        begin
        if RowJustify = NoJustify then
          Justify := Centered   {default <TH> is centered}
        else Justify := RowJustify;
        CurrentStyle := CurrentStyle + [fsBold];
        end
      else
        begin
        if RowJustify = NoJustify then
          Justify := Left   {default <TD> is Left}
        else Justify := RowJustify;
        CurrentStyle := CurrentStyle - [fsBold];
        end;
      TmpJustify := GetJustify;     {see if there is Align override}
      if TmpJustify <> NoJustify then
        Justify := TmpJustify;
      VAlign := GetVAlign(RowVAlign);
      CellObj := TCellObj.Create(MasterList, VAlign, Attributes);
      SectionList := CellObj.Cell;
      SkipWhiteSpace;
      Next;
      NoPSpace := True;
      DoBody(0, TableTermSet);
      end;
    CaptionSy:
      begin
      if InHref then DoAEnd;
      while StackIndex > SaveIndex do
        PopFont;          {terminate any <font> introduced in table}
      CurrentStyle := SaveStyle;
      NoBreak := False;
      AddSection;
      if Attributes.Find(AlignSy, T) then
        Table.TopCaption := Lowercase(T.Name) <> 'bottom';
      SectionList := Table.Caption.Cell;
      Justify := Centered;
      Next;
      DoBody(0, TableTermSet);

      SectionList.Add(Section);
      Section := Nil;
      SectionList := Nil;
      if Sy = CaptionEndSy then Next;  {else it's TDSy, THSy, etc}
      end;
    TREndSy:
      begin
      if InHref then DoAEnd;
      if Assigned(Row) then
        begin
        AddSection;
        Table.Rows.Add(Row);
        Row := Nil;
        end;
      Next;
      end;
    TRSy:
      begin
      if InHref then DoAEnd;
      if Assigned(Row) then
        begin
        AddSection;
        Table.Rows.Add(Row);
        end;
      Row := TCellList.Create;
      RowJustify := GetJustify;
      RowVAlign := GetVAlign(AMiddle);  
      Row.DoAttributes(Attributes);
      Next;
      end;
    TDEndSy, THEndSy:
      begin AddSection; Next; end;
    TextSy:
      begin
      JunkSaveSectionList := SectionList;
      SectionList := SaveSectionList;   {the original one}
      DoBody(0, TableTermSet);
      SectionList.Add(Section);
      Section := Nil;
      SectionList := JunkSaveSectionList;
      end;
    else Next;  {ignore all else}
    end;
if InHref then DoAEnd;
while StackIndex > SaveIndex do
  PopFont;          {terminate any <font> introduced in table}
AddSection;
if Assigned(Row) then
  Table.Rows.Add(Row);
SectionList := SaveSectionList;
SectionList.Add(Table);
CurrentStyle := SaveStyle;
NoBreak := SaveNoBreak;
Justify := SaveJustify;
Next;
end;

procedure ThlParser.GetOptions(Select: TListBoxFormControlObj);
 {get the <option>s for Select form control}
var
  Selected, InOption: boolean;
  Value, S: string[255];
  T: TAttribute;
  SaveNoBreak: boolean;
begin
SaveNoBreak := NoBreak;
NoBreak := False;
Next;
S := '';  Value := '';
Selected := False;  InOption := False;
while not (Sy in[SelectEndSy, EofSy]) do
  begin
  case Sy of
    OptionSy, OptionEndSy:
      begin
      S := Trim(S);
      if S <> '' then
        begin
        if InOption then
          Select.AddStr(S, Value, Selected);
        S := '';
        Value := '';
        Selected := False;
        end;
      InOption := Sy = OptionSy;
      if InOption then
        begin
        Selected := Attributes.Find(SelectedSy, T);
        if Attributes.Find(ValueSy, T) then
          Value := T.Name;
        end;
      end;
    TextSy: if InOption then
              S := S+LCToken.S;  
    end;
  Next;
  end;
if InOption then
  begin
  S := Trim(S);
  if S <> '' then
    Select.AddStr(S, Value, Selected);
  end;
NoBreak := SaveNoBreak;
end;

function GetColor(S: String; Var Value: TColor): boolean;
const
  ColorValues: array[1..16] of TColor =
     (clBLACK, clMAROON, clGREEN, clOLIVE, clNAVY, clPURPLE, clTEAL, clGRAY,
      clSILVER, clRED, clLIME, clYELLOW, clBLUE, clFUCHSIA, clAQUA, clWHITE);

const
  Colors: array[1..16] of string[7] =
     ('BLACK', 'MAROON', 'GREEN', 'OLIVE', 'NAVY', 'PURPLE', 'TEAL', 'GRAY',
      'SILVER', 'RED', 'LIME', 'YELLOW', 'BLUE', 'FUCHSIA', 'AQUA', 'WHITE');
var
  Red, Blue: integer;
  I: integer;
  S1: string[10];
begin
GetColor := False;
if Length(S) > 0 then
  begin
  S1 := Uppercase(S);
  for I := 1 to 16 do
    if S1 = Colors[I] then
      begin
      Value := ColorValues[I];
      GetColor := True;
      Exit;
      end;
  try
    I := Pos('#', S);   {apparently # is allowed}
    if I > 0 then Delete(S, I, 1);
    while Length(S) < 6 do
      S := S+'0';
    Value := StrToInt('$'+S);  {but bytes are backwards!}
    Red := Value and $FF;
    Blue := Value and $FF0000;
    Value := (Value and $00FF00) + (Red shl 16) + (Blue shr 16);
    GetColor := True;
  except
  end;
  end;
end;

{----------------DoMap}
procedure ThlParser.DoMap;
var
  Item: TMapItem;
  T: TAttribute;
  ErrorCnt: integer;
begin
Item := TMapItem.Create;
ErrorCnt := 0;
try
  if Attributes.Find(NameSy, T) then
    Item.MapName := Uppercase(T.Name);
  Next;
  while (Sy <> MapEndSy) and (Sy <> EofSy) and (ErrorCnt < 3) do
    begin
    if Sy = AreaSy then Item.AddArea(Attributes)
    else if Sy <> TextSy then
      Inc(ErrorCnt);
    Next;
    end;
  if Sy = MapEndSy then MasterList.MapList.Add(Item)
    else Item.Free;
except
  Item.Free;
  Raise;
  end;
Next;
end;

procedure ThlParser.DoScript(Ascript: TScriptEvent);
const
  Block = 32500;
var
  Lang, AName: string; 
  T: TAttribute;
  Buffer: PChar;
  Pos, Size: integer;

  procedure AddText(const S: string);
  begin
  if Pos + Length(S) >= Size then
    begin   {Delphi 2,3, add to buffer}
    ReAllocMem(Buffer, Size+10000);
    Inc(Size, 10000);
    end;
  Move(S[1], Buffer[Pos], Length(S));
  Inc(Pos, Length(S));
  end;

  procedure Next1;
    {Special Next routine to get the next token}
    procedure GetTag1;  {simplified 'Pick up a Tag' routine}
    var
      Count: integer;
    begin
    LCToken.AddChar('<', SIndex);  
    GetCh;
    Sy := CommandSy;   {catch all}
    Count := 0;
    while (Ch in ['A'..'Z', '/']) and (Count <= 6) do
      begin
      LCToken.AddChar(LCh, SIndex);  
      GetCh;
      Inc(Count);
      end;
    if LCh = '>' then
      begin
      LCToken.AddChar(LCh, SIndex);  
      GetCh;
      end;
    if CompareText(LCToken.S, '</script>') = 0 then    
      Sy := ScriptEndSy;
    end;

  begin  {already have fresh character loaded here}
  LCToken.Clear;    
  if LCh = EofChar then Sy := EofSy
  else if LCh = ^M then
    begin
    Sy := EolSy;
    GetCh;
    end
  else if LCh = '<' then
     GetTag1
  else
    begin
    Sy := TextSy;
    while (Length(LCToken.S) < 100) and  not (LCh in [^M, '<', EofChar]) do 
      begin
      LCToken.AddChar(LCh, SIndex);  
      GetCh;
      end;
    end;
  end;

begin
try
  if Assigned(AScript) then
    begin
    InScript := True;
    if Attributes.Find(LanguageSy, T) then
      Lang := T.Name
    else Lang := '';
    if Attributes.Find(NameSy, T) then 
      AName := T.Name
    else AName := '';                  

    GetMem(Buffer, Block);
    Pos := 0;
    Size := Block;
    try
      Next1;
      while (Sy <> ScriptEndSy) and (Sy <> EofSy) do
        begin
        if Sy = EolSy then AddText(^M^J)
        else
          AddText(LCToken.S);  
        Next1;
        end;
      AddText(#0);
      ReAllocMem(Buffer, Size);
      AScript(CallingObject, AName, Lang, Buffer);
    except
      FreeMem(Buffer);
      Raise;
      end;
    end
  else
    begin
    repeat
      Next1;
    until Sy in [ScriptEndSy, EofSy];
    end;
finally
  InScript := False;
  end;
end;

{----------------DoCommonSy}
procedure ThlParser.DoCommonSy(Lev: integer);
var
  I: integer;
  TxtArea: TTextAreaFormControlObj;
  FormControl: TFormControlObj;
  T: TAttribute;
  TmpJustify, LastAlign: JustifyType;
  Tmp: string;

  function IncrementFont(Sy: Symb; Pre: boolean): boolean;
  var
    NewSize: integer;

    function GetSizeIndex(Pre: boolean; Size: integer): integer;
    begin
    for Result := 1 to 7 do
      if Pre and (Size = PreFontConv[Result]) then Exit
      else if Size = FontConv[Result] then Exit;
    Result := -1;
    end;

  begin
  Result := False;
  NewSize := GetSizeIndex(Pre, FontStack[StackIndex].NormalSize);
  if (Sy = BigSy) then
    begin
    if (NewSize in [1..6]) then Inc(NewSize);
    end
  else
    if NewSize in [2..7] then Dec(NewSize);   

  if PushNewFont then
    begin
    if Pre then NewSize := PreFontConv[NewSize]
    else NewSize := FontConv[NewSize];
    FontStack[StackIndex].SetNormalSize(MasterList, NewSize);
    Result := True;
    end;
  end;

  function ChangeTheFont(Sy: Symb; Pre: boolean): boolean;
  var
    FaceName: string[50];
    NewColor: TColor;
    NewSize, I, K: integer;
    FontResults: set of (Face, Colr, Siz);
  begin
  FontResults := [];
  NewSize := 0;  {get rid of warning}
  for I := 0 to Attributes.Count-1 do
    with TAttribute(Attributes[I]) do
      case Which of
        SizeSy:
          begin
          if (Length(Name) >= 2) and (Name[1] in ['+', '-']) then
            Value := BaseFontSize + Value;
          NewSize := IntMax(1, IntMin(7, Value)); {limit 1..7}
          if (Sy = BaseFontSy) then BaseFontSize := NewSize;
          Include(FontResults, Siz);
          end;
        ColorSy:
          if (Sy <> BaseFontSy) and GetColor(Name, NewColor) then Include(FontResults, Colr);
        FaceSy:
          if (Sy <> BaseFontSy) and (Name <> '') then
            begin
            FaceName := Name;
            K := Pos(',', FaceName);
            if K > 0 then
              Delete(FaceName, K, 255);
            FaceName := Trim(FaceName);
            if FaceName <> '' then
              Include(FontResults, Face);
            end;
        end;
  Result := False;
  if ((Sy <> BasefontSy) or (SectionList.Count = 0) and
          (SectionList = MasterList)) and  {new font only if at start for Basefont}
       PushNewFont and (FontResults <> []) then
    with FontStack[StackIndex] do
      begin
      if Colr in FontResults then
        Color := NewColor or $2000000;
      if Siz in FontResults then
        begin
        if Pre then NewSize := PreFontConv[NewSize]
        else NewSize := FontConv[NewSize];
        SetNormalSize(MasterList, NewSize);
        end;
      if Face in FontResults then
        Name := FaceName;
      Result := True;
      end;
  end;

  procedure DoPreSy;
  var
    S: TokenObj;    
    Tmp: String;
    Done: boolean;
    I, InitialStackIndex: integer;

    procedure NewSection;
    begin
    Section.AddTokenObj(S, NoBreak);
    S.Clear; 
    SectionList.Add(Section);
    Section := TPreFormated.Create(MasterList, Lev, FontStack[StackIndex],
               CurrentUrlTarget, Left);
    end;

  begin
  S := TokenObj.Create;   
  try
    SectionList.Add(Section);
    PushNewFont;
    InitialStackIndex := StackIndex;   
    with FontStack[StackIndex] do
      begin
      Name := MasterList.PreFontName;
      SetNormalSize(MasterList, 10);
      Fixed := True;
      end;
    Section := TPreformated.Create(MasterList, Lev, FontStack[StackIndex],
               CurrentUrlTarget, Left);
    S.Clear;  
    PreFormat := True;
    Done := False;
    while not Done do
      case Ch of
        '&': begin
             Next;
             S.Concat(LCToken);  
             end;
        '<':
           begin
           Next;
           case Sy of
             PSy, BRSy:
               begin
               NewSection;
               if Ch = ^M then GetCh;
               end;

             PreEndSy, TDEndSy, THEndSy:
               Done := True;

             BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
                 USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
                 SSy, SEndSy, StrikeSy, StrikeEndSy:
               begin
               Section.AddTokenObj(S, NoBreak);
               S.Clear;   
               case Sy of
                 BSy, StrongSy:  CurrentStyle := CurrentStyle + [fsBold];
                 BEndSy, StrongEndSy:  CurrentStyle := CurrentStyle - [fsBold];
                 ISy, EmSy, CiteSy, VarSy:  CurrentStyle := CurrentStyle + [fsItalic];
                 IEndSy, EmEndSy,
                   CiteEndSy, VarEndSy:  CurrentStyle := CurrentStyle - [fsItalic];
                 USy:  CurrentStyle := CurrentStyle + [fsUnderline];
                 UEndSy:  CurrentStyle := CurrentStyle - [fsUnderline];
                 SSy, StrikeSy:  CurrentStyle := CurrentStyle + [fsStrikeOut];
                 SEndSy, StrikeEndSy:  CurrentStyle := CurrentStyle - [fsStrikeOut];
                 end;

               TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
               end;

             FontSy, BaseFontSy:
               begin
               Section.AddTokenObj(S, NoBreak);
               S.Clear;
               if ChangeTheFont(Sy, True) then
                 TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
               end;
             FontEndSy:
               if StackIndex > InitialStackIndex then  
                 begin
                 PopFont;
                 Section.AddTokenObj(S, NoBreak);
                 S.Clear;
                 TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
                 end;
             ASy:
               for I := 0 to Attributes.Count-1 do
                 with TAttribute(Attributes[I]) do
                   case Which of
                     NameSy:
                       if Name <> '' then
                         begin
                         Tmp := UpperCase(Name);
                         {Author may have added '#' by mistake}
                         if (Length(Tmp) > 0) and (Tmp[1] = '#') then
                           Delete(Tmp, 1, 1);
                         NameList.AddObject(Tmp, Section);
                         end;
                     HRefSy:
                       begin
                       Section.AddTokenObj(S, NoBreak);
                       S.Clear;
                       if InHref then DoAEnd;
                       if Name <> '' then  {also have a string}
                         begin
                         if Attributes.Find(TargetSy, T) then
                           CurrentUrlTarget.Assign(Name, T.Name)
                         else CurrentUrlTarget.Assign(Name, '');
                         InHref := True;
                         PushNewFont;
                         with FontStack[StackIndex] do
                           begin
                           Style := Style + MasterList.UnLine;
                           Color := MasterList.HotSpotColor;
                           end;
                         end;
                       Section.HRef(HRefSy, MasterList, CurrentUrlTarget, FontStack[StackIndex]);
                       end;
                     end;
             AEndSy:
               begin
               Section.AddTokenObj(S, NoBreak);
               S.Clear;
               DoAEnd;
               end;
             ImageSy:
               begin
               Section.AddTokenObj(S, NoBreak);
               TSection(Section).AddImage(Attributes, SectionList, TagIndex, NoBreak);
               S.Clear;
               end;
             InputSy, SelectSy:
               begin
               Section.AddTokenObj(S, NoBreak);
               FormControl := TSection(Section).AddFormControl(Sy, MasterList,
                           Attributes, SectionList, TagIndex, NoBreak);
               if Sy = SelectSy then
                 GetOptions(FormControl as TListBoxFormControlObj);
               S.Clear;;
               end;
             TextAreaSy:
               Begin
               Section.AddTokenObj(S, NoBreak);
               TxtArea := TSection(Section).AddFormControl(TextAreaSy, MasterList,
                          Attributes, SectionList, TagIndex, NoBreak) as TTextAreaFormControlObj;
               DoTextArea(TxtArea);
               S.Clear;
               end;
             FormSy:
               CurrentForm := ThtmlForm.Create(MasterList, Attributes);
             FormEndSy:
               CurrentForm := Nil;
             MapSy: DoMap;
             ScriptSy: DoScript(MasterList.ScriptEvent);
             end;
           end;
        ^M : begin NewSection; GetCh; end;
        EofChar : Done := True;
        else
          begin   {all other chars}
          S.AddChar(LCh, SIndex);  
          if Length(S.S) > 200 then
            begin
            Section.AddTokenObj(S, NoBreak);
            S.Clear;
            end;
          GetCh;
          end;
        end;
    Section.AddTokenObj(S, NoBreak);
    SectionList.Add(Section);
    Section := Nil;
    PreFormat := False;
    while StackIndex >= InitialStackIndex do   
      PopFont;
    Next;
  finally
    S.Free;   
    end;
  end;

  function CreateFont(HeadNmb: integer; OldFont: TMyFont): TMyFont;
  var
    F : TMyFont;
    Siz: integer;
  begin
  F := TMyFont.Create;
  F.Assign(OldFont);
  case HeadNmb of
    0: Siz := 12;     {0 is no heading}
    1: Siz := 24;
    2: Siz := 18; 
    3: Siz := 14;  
    4: Siz := 12;
    5: Siz := 10;
    6: Siz := 8;
    else Siz := 12;
  end;
  if HeadNmb > 0 then
    F.Style := F.Style + [fsBold];
  F.SetNormalSize(MasterList, Siz);
  Result := F;
  end;

begin
case Sy of
  TextSy :
    begin
    if not Assigned(Section) then
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
    Section.AddTokenObj(LCToken, NoBreak);
    Next;
    end;
  ImageSy:
    begin
    if not Assigned(Section) then
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
    TSection(Section).AddImage(Attributes, SectionList, TagIndex, NoBreak);
    Next;
    end;
  InputSy, SelectSy:
    begin
    if not Assigned(Section) then
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
    FormControl := TSection(Section).AddFormControl(Sy, MasterList, Attributes, SectionList, TagIndex, NoBreak);
    if Sy = SelectSy then
      GetOptions(FormControl as TListBoxFormControlObj);
    Next;
    end;
  TextAreaSy:
    Begin
    if not Assigned(Section) then
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
    TxtArea := TSection(Section).AddFormControl(TextAreaSy, MasterList,
               Attributes, SectionList, TagIndex, NoBreak) as TTextAreaFormControlObj; 
    DoTextArea(TxtArea);
    Next;
    end;
  TextAreaEndSy:   {a syntax error but shouldn't hang}
    Next;
  FormSy:
    begin
    CurrentForm := ThtmlForm.Create(MasterList, Attributes);
    Next;
    end;
  FormEndSy:
    begin
    CurrentForm := Nil;
    Next;
    end;
  PSy, PEndSy:
    begin
    SectionList.Add(Section);
    Section := Nil;
    if Not NoPSpace then
      SectionList.Add(TParagraphSpace.Create(MasterList));
    SkipWhiteSpace;
    LastAlign := FindAlignment;
    NoPSpace := True;
    Next;
    while Sy in [PSy, PEndSy] do
      begin           {recognize only the first <p>}
      LastAlign := FindAlignment;   {if a series of <p>, get last alignment}
      SkipWhiteSpace;
      NoPSpace := True;
      Next;
      end;
    Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
               CurrentUrlTarget, LastAlign);
    end;
  BRSy:
    begin
    if Assigned(Section) then
      TmpJustify := TSection(Section).BreakInfo(TagIndex, NoBreak)  {so <br> doesn't change justification}
    else TmpJustify := Justify;
    SectionList.Add(Section);
    Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
               CurrentUrlTarget, TmpJustify);
    Section.DoClearAttribute(Attributes);     {check for clear attribute}
    Next;
    end;
  NoBrSy, NoBrEndSy:
    begin
    NoBreak := Sy = NoBrSy;
    if Assigned(Section) then
      Section.AddTokenObj(LCToken, NoBreak);
    Next;
    end;
  WbrSy:
    begin
    if Assigned(Section) and NoBreak then
      Section.AddChar(' ', TagIndex, NoBreak);
    Next;
    end;
  BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
      USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
      SubSy, SubEndSy, SupSy, SupEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy:
    begin
    case Sy of
      BSy, StrongSy:  CurrentStyle := CurrentStyle + [fsBold];
      BEndSy, StrongEndSy:  CurrentStyle := CurrentStyle - [fsBold];
      ISy, EmSy, CiteSy, VarSy:  CurrentStyle := CurrentStyle + [fsItalic];
      IEndSy, EmEndSy,
        CiteEndSy, VarEndSy:  CurrentStyle := CurrentStyle - [fsItalic];
      USy:  CurrentStyle := CurrentStyle + [fsUnderline];
      UEndSy:  CurrentStyle := CurrentStyle - [fsUnderline];
      SSy, StrikeSy:  CurrentStyle := CurrentStyle + [fsStrikeOut];
      SEndSy, StrikeEndSy:  CurrentStyle := CurrentStyle - [fsStrikeOut];
      SubEndSy, SupEndSy: CurrentSScript := Normal;
      SubSy, SupSy:
        begin
        if not Assigned(Section) then
          Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                     CurrentUrlTarget, Justify);
        if Sy = SubSy then CurrentSScript := SubSc
          else CurrentSScript := SupSc;
        end;
      end;

    if Assigned(Section) then    {CurrentStyle used in ChangeFont}
      TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
    Next;
    end;
  TTSy, CodeSy, KbdSy, SampSy:
    begin
    if PushNewFont then
      begin
      with FontStack[StackIndex] do
        begin
        Name := MasterList.PreFontName;
        SetNormalSize(MasterList, 10);
        Fixed := True;
        end;
      if Assigned(Section) then
        TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
      end;
    Next;
    end;
  TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, FontEndSy, BigEndSy, SmallEndSy:
    begin
    PopFont;
      if Assigned(Section) then
        TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
    Next;
    end;
  FontSy, BaseFontSy:
    begin
    if ChangeTheFont(Sy, False) and Assigned(Section) then
      TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
    Next;
    end;
  BigSy, SmallSy:
    begin
    if IncrementFont(Sy, False) and Assigned(Section) then
      TSection(Section).ChangeFont(MasterList, FontStack[StackIndex]);
    Next;
    end;
  AddressSy:
    begin
    SectionList.Add(Section);
    PushNewFont;
    with FontStack[StackIndex] do
      Style := Style + [fsItalic];
    Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
               CurrentUrlTarget, Justify);
    Next;
    end;
  AddressEndSy:
    begin
    SectionList.Add(Section);
    Section := Nil;
    PopFont;
    Next;
    end;
  ASy:
    begin
    for I := 0 to Attributes.Count-1 do
      with TAttribute(Attributes[I]) do
        case Which of
          NameSy:
            if Name <> '' then
              begin
              if not Assigned(Section) then
                Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                           CurrentUrlTarget, Justify);
              Tmp := UpperCase(Name);
              {Author may have added '#' by mistake}
              if (Length(Tmp) > 0) and (Tmp[1] = '#') then
                Delete(Tmp, 1, 1);
              NameList.AddObject(Tmp, Section);
              end;
          HRefSy:
            begin
            if InHref then DoAEnd;
            if Name <> '' then  {also have a string}
              begin
              if Attributes.Find(TargetSy, T) then
                CurrentUrlTarget.Assign(Name, T.Name)
              else CurrentUrlTarget.Assign(Name, '');
              InHref := True;
              PushNewFont;
              with FontStack[StackIndex] do
                begin
                Style := Style + MasterList.UnLine;
                Color := MasterList.HotSpotColor;
                end;
              if Assigned(Section) then
                Section.HRef(HRefSy, MasterList, CurrentUrlTarget, FontStack[StackIndex]);
              end;
            end;
          end;
    Next;
    end;
  AEndSy:
    begin
    DoAEnd;
    Next;
    end;
  HeadingSy:
    begin
    if StackIndex < MaxStack then
      begin
      SectionList.Add(Section);
      Inc(StackIndex);
      FontStack[StackIndex] := CreateFont(Value, FontStack[StackIndex-1]);
      SectionList.Add(THeadingSpace.Create(MasterList, Value));
      Section := TSection.Create(MasterList, Lev, FontStack[StackIndex],
                 CurrentUrlTarget, FindAlignment);
      end;
    Next;
    end;
  HeadingEndSy:
    begin
    if StackIndex > 1 then
      begin
      SectionList.Add(Section);
      SectionList.Add(THeadingSpace.Create(MasterList, Value));
      Section := Nil;
      PopFont;
      end;
    Next;
    end;
  PreSy:  DoPreSy;
  TableSy: DoTable(Lev);
  MapSy: DoMap;
  ScriptSy: begin DoScript(MasterList.ScriptEvent); Next; end;
  end;
end;   {DoCommon}

{-------------DoLists}
procedure ThlParser.DoLists(Level: integer; Sym: Symb; const TermSet: SymbSet);
var
  LineCount, CurrentLevel: integer;
  T: TAttribute;
  Plain: boolean;
  Index: char;   

begin
LineCount := 1;
Index := '1';
Plain := False;
if (Sym = OLSy) then
  begin
  if Attributes.Find(StartSy, T) then
    if T.Value >= 0 then LineCount := T.Value;
  if Attributes.Find(TypeSy, T) and (T.Name <> '') then
    Index := T.Name[1];
  end
else Plain := (Sym = ULSy) and Attributes.Find(PlainSy, T);
CurrentLevel := Level;
SectionList.Add(Section);
Section := Nil;
Next;
if Sy in [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy, BlockQuoteEndSy] then
  Exit;    {guard against <ul></ul> and similar combinations}
repeat
  case Sy of
    LISy:
      begin
      SectionList.Add(Section);
      if Sym = OLSy then
        begin
        Section := TOListItem.Create(MasterList, Level, LineCount, Index,
                    FontStack[StackIndex], CurrentUrlTarget);
        Inc(LineCount);
        end
      else Section := TUlistItem.Create(MasterList, Level, FontStack[StackIndex],
                      CurrentUrlTarget);
      if (Sym = ULSy) and Plain then
        TUlistItem(Section).Plain := True;
      CurrentLevel := Level;
      SkipWhiteSpace;
      Next;
      if Sy = PSy then Next;
      end;
    DTSy, DDSy:
      begin
      SectionList.Add(Section);
      if Sy = DTSy then
        CurrentLevel := Level-1
      else CurrentLevel := Level;
      Section := TDListItem.Create(MasterList, CurrentLevel, FontStack[StackIndex],
                 CurrentUrlTarget);
      Next;
      end;
    OLSy, ULSy, DirSy, MenuSy, DLSy:
      begin
      DoLists(Level+1, Sy, TermSet);
      Next;
      end;
    BlockQuoteSy:
      begin
      SectionList.Add(Section);
      Section := Nil;
      DoLists(Level+1, Sy, TermSet);
      Next;
      end;
    DivSy, CenterSy:
      DoBody(CurrentLevel, [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy,
             BlockQuoteEndSy, EofSy]+TermSet);
    HRSy:
      begin
      SectionList.Add(Section);
      SectionList.Add(THorzLine.Create(MasterList, Attributes));
      Section := Nil;
      Next;
      end;
  TableSy:
      begin
      if Assigned(Section) then
        TSection(Section).BreakInfo(TagIndex, NoBreak);
      DoTable(CurrentLevel);   
      end;

    TextSy, BRSy, PSy, PEndSy,
    BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
        USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
        SubSy, SubEndSy, SupSy, SupEndSy, SSy, SEndSy, StrikeSy, StrikeEndSy, 
    TTSy, CodeSy, KbdSy, SampSy,  TTEndSy, CodeEndSy, KbdEndSy, SampEndSy,
    NameSy, HRefSy, ASy, AEndSy,
    HeadingSy, HeadingEndSy, AddressSy, AddressEndSy, PreSy,
    InputSy, FormSy, FormEndSy, TextAreaSy, TextAreaEndSy, SelectSy,
    ImageSy, FontSy, FontEndSy, BaseFontSy, BigSy, BigEndSy, SmallSy,
    SmallEndSy, MapSy, ScriptSy, NoBrSy, NoBrEndSy, WbrSy:
      DoCommonSy(CurrentLevel);
    else if Sy in TermSet then Exit
      else Next;
    end;
Until (Sy in [OLEndSy, ULEndSy, DirEndSy, MenuEndSy, DLEndSy,
             BlockQuoteEndSy, EofSy]);
SectionList.Add(Section);
Section := Nil;
end;

{----------------DoBase}
procedure ThlParser.DoBase;
var
  I: integer;
begin
with Attributes do
  for I := 0 to Count-1 do
    with TAttribute(Attributes[I]) do
      if Which = HrefSy then
        Base := Name
      else if Which = TargetSy then
        BaseTarget := Name;
Next;
end;

procedure ThlParser.DoSoundEvent;
begin
SoundEvent(CallingObject, ATName, ALoop, False);
end;

{----------------DoSound}
procedure ThlParser.DoSound;
var
  T, T1: TAttribute;
begin
if Assigned(SoundEvent) and Attributes.Find(SrcSy, T) then
  begin
  if Attributes.Find(LoopSy, T1) then ALoop := T1.Value
    else ALoop := 1;
  ATName := T.Name;
  TParseThread(ParseThread).Synchronize(
    {$IFDEF HL_LAZARUS}@{$ENDIF}DoSoundEvent);
  end;
Next;
end;

function ThlParser.TranslateCharset(DefCharset: TFontCharset;
  const {$IFDEF HL_LAZARUS}NewContent{$ELSE}Content{$ENDIF}: string): TFontCharset;
type
  XRec = record S: string; CSet: TFontCharset; end;
const
  MaxX = 14;
  XTable: array[1..MaxX] of XRec =
     ((S:'1252';    CSet:ANSI_CHARSET),
      (S:'8859-1';    CSet:ANSI_CHARSET),
      (S:'1253';    CSet:GREEK_CHARSET),
      (S:'8859-7';    CSet:GREEK_CHARSET),
      (S:'1250';    CSet:EASTEUROPE_CHARSET),
      (S:'8859-2';    CSet:EASTEUROPE_CHARSET),
      (S:'1251';    CSet:RUSSIAN_CHARSET),
      (S:'8859-5';    CSet:RUSSIAN_CHARSET),
      (S:'koi8-r';    CSet:RUSSIAN_CHARSET),
      (S:'1254';    CSet:TURKISH_CHARSET),
      (S:'8859-3';    CSet:TURKISH_CHARSET),
      (S:'8859-9';    CSet:TURKISH_CHARSET),
      (S:'1257';    CSet:BALTIC_CHARSET),
      (S:'8859-4';    CSet:BALTIC_CHARSET));
var
  I: integer;
begin
Result := DefCharset;
for I := 1 to MaxX do
  if Pos(XTable[I].S,
    Lowercase({$IFDEF HL_LAZARUS}NewContent{$ELSE}Content{$ENDIF})) > 0
  then
    Begin
    Result := XTable[I].CSet;
    Break;
    end;
end;

procedure ThlParser.DoMetaEvent;
begin
MetaEvent(CallingObject, HttpEq, ATName, Content);
end;

{----------------DoMeta}
procedure ThlParser.DoMeta(Sender: TObject);
var
  T: TAttribute;
  {$ifdef ver100_plus}
  Charset: TFontCharset;
  {$endif}
begin
if Attributes.Find(HttpEqSy, T) then HttpEq := T.Name
  else HttpEq := '';
if Attributes.Find(NameSy, T) then ATName := T.Name
  else ATName := '';
if Attributes.Find(ContentSy, T) then Content := T.Name
  else Content := '';
{$ifdef ver100_plus}
if (Sender is ThtmlLite) and (CompareText(HttpEq, 'content-type') = 0) then  
  begin
  CharSet := TranslateCharset(TSectionList(SectionList).Charset, Content);
  FontStack[StackIndex].Charset := Charset;
  end;
{$endif}
if Assigned(MetaEvent) then
  TParseThread(ParseThread).Synchronize({$IFDEF HL_LAZARUS}@{$ENDIF}DoMetaEvent);
Next;
end;

{----------------DoTitle}
procedure ThlParser.DoTitle;
begin
Title := '';
Next;
while Sy = TextSy do
  begin
  Title := Title+LCToken.S;
  Next;
  end;
end;

{-------------DoBody}
procedure ThlParser.DoBody(Level: integer; const TermSet: SymbSet);
var
  I, SaveIndent: integer;  
  Val: TColor;
  T: TAttribute;
  SaveJustify: JustifyType;
  S: string[10];
  SaveSy: Symb;

begin
repeat
  case Sy of
    TextSy, BRSy, PSy, PEndSy,
    NameSy, HRefSy, ASy, AEndSy,
    BSy, ISy, BEndSy, IEndSy, EmSy, EmEndSy, StrongSy, StrongEndSy,
        USy, UEndSy, CiteSy, CiteEndSy, VarSy, VarEndSy,
        SubSy, SubEndSy, SupSy, SupEndSy,  SSy, SEndSy, StrikeSy, StrikeEndSy,
    TTSy, CodeSy, KbdSy, SampSy,  TTEndSy, CodeEndSy, KbdEndSy, SampEndSy,
    HeadingSy, HeadingEndSy, AddressSy, AddressEndSy, PreSy, TableSy,
    InputSy, FormSy, FormEndSy, TextAreaSy, TextAreaEndSy, SelectSy,
    ImageSy, FontSy, FontEndSy, BaseFontSy, BigSy, BigEndSy, SmallSy,
    SmallEndSy, MapSy, ScriptSy, NoBrSy, NoBrEndSy, WbrSy:
      DoCommonSy(Level);
    BodySy:
      begin
      if SectionList.Count = 0 then   {make sure we're at beginning}
        begin
        Section.Free;   {Will start with a new section}
        for I := 0 to Attributes.Count-1 do
          with TAttribute(Attributes[I]) do
            case Which of
              BackgroundSy: MasterList.SetBackgroundBitmap(Name);
              TextSy: if GetColor(Name, Val) then
                begin
                FontStack[StackIndex].Color := Val or $2000000;
                MasterList.FontColor := Val or $2000000;
                end;
              BGColorSy: if GetColor(Name, Val) then MasterList.SetBackGround(Val or $2000000);
              LinkSy: if GetColor(Name, Val) then MasterList.HotSpotColor := Val or $2000000;
              VLinkSy: if GetColor(Name, Val) then MasterList.LinkVisitedColor := Val or $2000000;
              OLinkSy: if GetColor(Name, Val) then
                 begin
                 MasterList.LinkActiveColor := Val or $2000000;
                 MasterList.LinksActive := True;
                 end;
              MarginWidthSy:   
                if CallingObject is ThtmlLite then
                  ThtmlLite(CallingObject).FMarginWidthX := IntMin(IntMax(0,Value), 200);
              MarginHeightSy:
                if CallingObject is ThtmlLite then
                  ThtmlLite(CallingObject).FMarginHeightX := IntMin(IntMax(0,Value), 200);
            end;
        Section := TSection.Create(MasterList, Level, FontStack[1], Nil, Justify);
        end;
      Next;
      end;
    HRSy:
      begin
      SectionList.Add(Section);
      SectionList.Add(THorzLine.Create(MasterList, Attributes));
      Section := Nil;
      Next;
      end;
    OLSy, ULSy, DirSy, MenuSy, DLSy:
      begin
      DoLists(1, Sy, TermSet);
      Next;
      end;
    LiSy:
      if Level = 0 then
        begin
        SectionList.Add(Section);
        SaveIndent := ListIndent;
        ListIndent := SmallListIndent;
        Section := TUlistItem.Create(MasterList, 1, FontStack[StackIndex],
                      CurrentUrlTarget);
        SkipWhiteSpace;
        Next;
        while Sy in [TextSy, NoBrSy, NoBrEndSy, WbrSy, BSy, ISy, BEndSy, IEndSy,
                     EmSy, EmEndSy, StrongSy, StrongEndSy, USy, UEndSy, CiteSy,
                     CiteEndSy, VarSy, VarEndSy, SubSy, SubEndSy, SupSy, SupEndSy,
                     SSy, SEndSy, StrikeSy, StrikeEndSy, TTSy, CodeSy, KbdSy, SampSy,
                     TTEndSy, CodeEndSy, KbdEndSy, SampEndSy, FontEndSy, BigEndSy,
                     SmallEndSy, BigSy, SmallSy, ASy, AEndSy] do
          begin
          DoCommonSy(1);
          end;
        SectionList.Add(Section);
        Section := Nil;
        ListIndent := SaveIndent;
        end
      else Next;
    BlockQuoteSy:
      begin
      SectionList.Add(Section);
      Section := Nil;
      DoLists(1, Sy, TermSet);
      Next;
      end;
    DivSy, CenterSy:
      begin
      SaveSy := Sy;
      SectionList.Add(Section);
      SaveJustify := Justify;
      if SaveSy = CenterSy then
        Justify := Centered
      else
        if Attributes.Find(AlignSy, T) then
          begin
          S := LowerCase(T.Name);
          if S = 'left' then Justify := Left
          else if S = 'center' then Justify := Centered
          else if S = 'right' then Justify := Right;
          end;
      Section := TSection.Create(MasterList, Level, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
      Next;
      DoBody(Level, [CenterEndSy, DivEndSy]+TermSet);
      SectionList.Add(Section);
      Justify := SaveJustify;
      Section := TSection.Create(MasterList, Level, FontStack[StackIndex],
                 CurrentUrlTarget, Justify);
      if Sy in [CenterEndSy, DivEndSy] then
        Next;
      end;
    TitleSy:
      DoTitle;
    BgSoundSy:
      DoSound;
    MetaSy:
      DoMeta(CallingObject);
    BaseSy:
      DoBase;
    else if Sy in TermSet then Exit
    else Next;
    end;
Until (Sy = EofSy);
Next;
end;

procedure ThlParser.ParseInit(ASectionList: TList; ANameList: TStringList; AIncludeEvent: TIncludeType);
begin
SectionList := TSectionList(ASectionList);
MasterList := TSectionList(SectionList);
CallingObject := TSectionList(ASectionList).TheOwner;
IncludeEvent := AIncludeEvent;
NameList := ANameList;
PreFormat := False;
StackIndex := 1;
FontStack[1] := TMyFont.Create;
FontStack[1].Name := MasterList.FontName;
FontStack[1].Color := MasterList.FontColor;
FontStack[1].SetNormalSize(MasterList, 12);
{$ifdef ver100_plus}
FontStack[1].Charset := TSectionList(SectionList).Charset;
{$endif}
CurrentURLTarget := TUrlTarget.Create;
InHref := False;
BaseFontSize := 3;

Title := '';
Base := '';
BaseTarget := '';
Justify := Left;
CurrentStyle := [];
CurrentForm := Nil;
Section := TSection.Create(MasterList, 0, FontStack[1], Nil, Justify);
Attributes := TAttributeList.Create;
SIndex := -1;
InScript := False;
NoPSpace := False;
NoBreak := False;
end;

{----------------ThlParser.HtmlParseString}
procedure ThlParser.HtmlParseString(ASectionList: TList; ANameList: TStringList;
                   AIncludeEvent: TIncludeType; ASoundEvent: TSoundType;
                   AMetaEvent: TMetaType);
begin
LoadStyle := lsPrimary;
ParseInit(ASectionList, ANameList, AIncludeEvent);
SoundEvent := ASoundEvent;
MetaEvent := AMetaEvent;
Buffer := TParseThread(ParseThread).Buffer;
BuffEnd := TParseThread(ParseThread).BuffEnd;
try
  try
    GetCh; {get the reading started}
    Next;
    DoBody(0, []);
  except
    On EParseError do;   {ignore this error}
  end;
finally
  Attributes.Free;
  if Assigned(Section) then
    SectionList.Add(Section);
  while StackIndex >= 1 do
    begin
    FontStack[StackIndex].Free;
    Dec(StackIndex);
    end;
  CurrentURLTarget.Free;
  end;
end;

{----------------DoText}
procedure ThlParser.DoText;        
var
  S: TokenObj;
  Done: boolean;

  procedure NewSection;
  begin
  Section.AddTokenObj(S, NoBreak);
  S.Clear;
  SectionList.Add(Section);
  Section := TPreFormated.Create(MasterList, 0, FontStack[StackIndex], CurrentURLTarget, Left);
  end;

begin
S := TokenObj.Create;
try
  SectionList.Add(Section);
  PushNewFont;
  with FontStack[StackIndex] do
    begin
    Name := MasterList.PreFontName;
    SetNormalSize(MasterList, 10);
    Fixed := True;
    end;
  Section := TPreformated.Create(MasterList, 0, FontStack[StackIndex],
             CurrentURLTarget, Left);
  PreFormat := True;
  Done := False;
  while not Done do
    case Ch of
      ^M : begin NewSection; GetCh; end;
      EofChar : Done := True;
      else
        begin   {all other chars}
        S.AddChar(LCh, SIndex);
        if Length(S.S) > 200 then
          begin
          Section.AddTokenObj(S, NoBreak);
          S.Clear;
          end;
        GetCh;
        end;
      end;
  Section.AddTokenObj(S, NoBreak);
  SectionList.Add(Section);
  Section := Nil;
  PreFormat := False;
  PopFont;
finally
  S.Free;
  end;
end;

{-------------HtmlParseTextString}
procedure ThlParser.HtmlParseTextString(ASectionList: TList; ANameList: TStringList);
begin
LoadStyle := lsPrimary;
ParseInit(ASectionList, ANameList, Nil);
SoundEvent := Nil;
MetaEvent := NIl;
Buffer := TParseThread(ParseThread).Buffer;
BuffEnd := TParseThread(ParseThread).BuffEnd;

try
  try
    GetCh; {get the reading started}
    DoText;
  except
    On EParseError do;   {ignore this error}
  end;

finally
  Attributes.Free;
  if Assigned(Section) then
    SectionList.Add(Section);
  while StackIndex >= 1 do
    begin
    FontStack[StackIndex].Free;
    Dec(StackIndex);
    end;
 end;   {finally}
end;

constructor ThlParser.Create;
begin
inherited;
LCToken := TokenObj.Create;
end;

destructor ThlParser.Destroy;
begin
LCToken.Free;
inherited;
end;


end.

