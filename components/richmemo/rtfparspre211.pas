Unit RTFParsPre211;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, Member of the
    Free Pascal development team

    This unit implements a RTF Parser.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

// modified: by Dmitry Boyarintsev 20-may-2010, fixing identation and code-style

{$mode objfpc}

interface

uses
  Classes, SysUtils;

{$i rtfdata.inc}

type
  TRTFErrorHandler = Procedure (s : string) of object;

  { TRTFParser }

  TRTFParser = class(TObject)
  private
    FOnRTFError   : TRTFerrorHandler;
    FfontList     : PRTFFont;
    FcolorList    : PRTFColor;
    FstyleList    : PRTFStyle;
    FrtfClass     : Integer;
    FrtfMajor     : Integer;
    FrtfMinor     : Integer;
    FrtfParam     : Integer;
    rtfTextBuf    : string [rtfBufSiz];
    rtfTextLen    : Integer;
    pushedChar    : Integer;               { pushback char if read too far }
    pushedClass   : Integer;      { pushed token info for RTFUngetToken() }
    pushedMajor   : Integer;
    pushedMinor   : Integer;
    pushedParam   : Integer;
    pushedTextBuf : String[rtfBufSiz];
    FStream       : TStream;
    ccb : array [0..rtfMaxClass] of TRTFFuncPtr;                { class callbacks }
    dcb : array [0..rtfMaxDestination] of TRTFFuncPtr;  { destination callbacks }
    readHook : TRTFFUNCPTR;
    FTokenClass: Integer;
    procedure Error (msg : String);
    procedure LookupInit ;
    procedure ReadFontTbl ;
    procedure ReadColorTbl;
    procedure ReadStyleSheet ;
    procedure ReadInfoGroup ;
    procedure ReadPictGroup ;
    function  CheckCM (Aclass, major: Integer) : Boolean;
    function  CheckCMM (Aclass, major, minor : Integer) : Boolean;
    function  CheckMM (major, minor : Integer) : Boolean;
    procedure Real_RTFGetToken;
    function  GetChar : Integer;
    procedure Lookup (S : String);
    function  GetFont (num : Integer) : PRTFFont;
    function  GetColor (num : Integer) : PRTFColor;
    function  GetStyle (num : Integer) : PRTFStyle;
    procedure setClassCallback (Aclass : Integer; Acallback : TRTFFuncPtr);
    function  GetClassCallback (Aclass : Integer) : TRTFFuncPtr;
    procedure SetDestinationCallback (ADestination : Integer; Acallback : TRTFFuncPtr);
    function  GetDestinationCallback (Adestination : Integer) : TRTFFuncPtr ;
    procedure SetStream (Astream : TStream);
  public
    constructor Create (AStream : TStream);
    destructor  Destroy; override;
    procedure GetReadHook (Var q : TRTFFuncPtr);
    function  GetToken : Integer;
    function  PeekToken : Integer;
    procedure ResetParser;
    procedure RouteToken;
    procedure SkipGroup;
    procedure StartReading;
    procedure SetReadHook (Hook : TRTFFuncPtr);
    procedure UngetToken;
    procedure SetToken (Aclass, major, minor, param : Integer; text : string);
    procedure ExpandStyle (n : Integer);
    function  GetRtfText: string;
    { Properties }
    property Colors [Index : Integer]: PRTFColor Read GetColor;
    property ClassCallBacks [AClass : Integer]: TRTFFuncptr
       read GetClassCallBack write SetClassCallback;
    property DestinationCallBacks [Adestination : Integer]: TRTFFuncptr
       read GetdestinationCallBack write SetdestinationCallback;
    property Fonts [Index : Integer]: PRTFFont Read GetFont;
    property OnRTFError : TRTFerrorHandler Read FOnRTFError Write FOnRTFError;
    property rtfClass : Integer Read FrtfClass;
    property rtfMajor : Integer Read FrtfMajor;
    property rtfMinor : Integer Read FrtfMinor;
    property rtfParam : Integer Read FrtfParam;
    property Stream : TStream Read FStream Write SetStream;
    property Styles [index : Integer] : PRTFStyle Read GetStyle;
  end;

Implementation

const
  EOF = -255;

{ ---------------------------------------------------------------------
         Utility functions
  ---------------------------------------------------------------------}

function Hash (s : String) : Integer;
var
  val,i : integer;
begin
  val:=0;
  for i:=1 to length(s) do
    val:=val+ord(s[i]);
  Hash:=val;
end;

function isalpha (s : integer) : Boolean;
begin
  Result:= ( (s>=ord('A')) and (s<=ord('Z')))
             or (((s>=ord('a')) and ((s<=ord('z')) )));
end;

function isdigit (s : integer) : Boolean;
begin
  Result:= ( (s>=ord('0')) and (s<=ord('9')) )
end;

function HexVal (c : Integer) : Integer;
begin
  if (c>=ord('A')) and (C<=ord('Z')) then inc (c,32);
  if c<ord ('A') then
    result:=(c - ord('0'))      { '0'..'9' }
  else
    result:= (c - ord('a') + 10);               { 'a'..'f' }
end;

{ ---------------------------------------------------------------------
       Initialize the reader.  This may be called multiple times,
       to read multiple files.  The only thing not reset is the input
       stream; that must be done with RTFSetStream().
  ---------------------------------------------------------------------}

constructor TRTFParser.Create (AStream : TStream);
Begin
  inherited create;
  { initialize lookup table }
  LookupInit;
  Fstream := Astream;
  FfontList :=nil;
  FcolorList:=nil;
  FstyleList :=nil;
  onrtferror:=nil;
  ResetParser;
end;

Procedure TRTFParser.ResetParser;
var
  cp : PRTFColor;
  fp : PRTFFont;
  sp : PRTFStyle;
  ep,eltlist : PRTFStyleElt;
  i : integer;
begin
  for i:=0 to rtfMaxClass-1 do
    setClassCallback(i, nil);
  for i:=0 to rtfMaxDestination-1 do
    SetDestinationCallback(i, nil);

  { install built-in destination readers }
  SetDestinationCallback (rtfFontTbl, @ReadFontTbl);
  SetDestinationCallback (rtfColorTbl, @ReadColorTbl);
  SetDestinationCallback (rtfStyleSheet, @ReadStyleSheet);
  SetDestinationCallback (rtfInfo, @ReadInfoGroup);
  SetDestinationCallback (rtfPict, @ReadPictGroup);

  SetReadHook (Nil);

  { dump old lists if necessary }

  while FfontList<>nil do begin
    fp := FfontList^.rtfNextFont;
    dispose (FfontList);
    FfontList := fp;
  end;

  while FcolorList<>nil do begin
    cp := FcolorList^.rtfNextColor;
    dispose (FcolorList);
    FcolorList := cp;
  end;

  while FstyleList<>nil do begin
    sp := FstyleList^.rtfNextStyle;
    eltList := FstyleList^.rtfSSEList;
    while eltList<>nil do begin
      ep:=eltList^.rtfNextSE;
      dispose(eltList);
      eltList:= ep;
    end;
    Dispose (FstyleList);
    FstyleList := sp;
  end;
  FrtfClass := -1;
  pushedClass := -1;
  pushedChar := EOF;

  { Reset the stream if it is assigned }
  if assigned (FStream) then
    FStream.seek(0,soFromBeginning);
End;


Destructor TRTFParser.Destroy;
var
  cp : PRTFColor;
  fp : PRTFFont;
  sp : PRTFStyle;
  ep,eltlist : PRTFStyleElt;
begin
  { Dump the lists. }
  while FfontList<>nil do begin
    fp := FfontList^.rtfNextFont;
    dispose (FfontList);
    FfontList := fp;
  end;

  while FcolorList<>nil do begin
    cp := FcolorList^.rtfNextColor;
    dispose (FcolorList);
    FcolorList := cp;
  end;

  while FstyleList<>nil do begin
    sp := FstyleList^.rtfNextStyle;
    eltList := FstyleList^.rtfSSEList;
    while eltList<>nil do begin
      ep:=eltList^.rtfNextSE;
      dispose(eltList);
      eltList:= ep;
    end;
    Dispose (FstyleList);
    FstyleList := sp;
  end;

  { Dump rest }
  inherited destroy;
end;


{ ---------------------------------------------------------------------
       Callback table manipulation routines
  ---------------------------------------------------------------------}

Procedure TRTFParser.SetClassCallback (Aclass : Integer; Acallback : TRTFFuncPtr);
begin
  if (aclass>=0) and (Aclass<rtfMaxClass) then
    ccb[Aclass]:= Acallback;
end;

function TRTFParser.GetClassCallback (Aclass : Integer) : TRTFFuncPtr;
Begin
  if (Aclass>=0) and (Aclass<rtfMaxClass) then
    GetClassCallback :=ccb[Aclass]
  else
    GetClassCallback:=nil;
end;

{ ---------------------------------------------------------------------
   Install or return a writer callback for a destination type
  ---------------------------------------------------------------------}

Procedure TRTFParser.SetDestinationCallback (ADestination : Integer; Acallback : TRTFFuncPtr);
Begin
  if (Adestination>=0) and (Adestination<rtfMaxDestination) then
    dcb[ADestination] := Acallback;
end;

function TRTFParser.GetDestinationCallback (Adestination : Integer) : TRTFFuncPtr ;
begin
  if (Adestination>=0) and (ADestination<rtfMaxDestination) then
    Result:=dcb[Adestination]
  else
    Result:=nil;
end;

{ ---------------------------------------------------------------------
       Token reading routines
  ---------------------------------------------------------------------}

{ Read the input stream, invoking the writer's callbacks where appropriate. }

Procedure TRTFParser.StartReading;
begin
  { Reset stream. }
  FStream.Seek (0,soFromBeginning);
  { Start reading. }
  while (GetToken<>rtfEOF) do
    RouteToken;
end;


{ Route a token.  If it's a destination for which a reader is
  installed, process the destination internally, otherwise
  pass the token to the writer's class callback. }
procedure TRTFParser.RouteToken;
var
  p : TRTFFuncPtr;
begin
  if (rtfClass < 0) or (rtfClass>=rtfMaxClass) then
    Error ('No such class : '+rtfTextBuf)
  else begin
    if (CheckCM (rtfControl, rtfDestination)) then begin
      { invoke destination-specific callback if there is one }
      p:=GetDestinationCallback (rtfMinor);
      if assigned(p) then begin
        p();
        exit
      end;
    end;
    { invoke class callback if there is one }
    p:= GetClassCallback (rtfClass);
    if assigned(p) then p();
  end;
end;


{ Skip to the end of the current group.  When this returns,
  writers that maintain a state stack may want to call their
  state unstacker; global vars will still be set to the group's
  closing brace. }
Procedure TRTFParser.SkipGroup;
Var
  level : Integer;
Begin
  level:= 1;
  while (GetToken<>rtfEOF) do
    if (rtfClass=rtfGroup) then begin
      if (rtfMajor=rtfBeginGroup) then
        inc(level)
      else if (rtfMajor=rtfEndGroup) then begin
        dec(level);
        if (level < 1) then Exit;       { end of initial group }
      end;
    end;
End;

{ Read one token.  Call the read hook if there is one.  The
  token class is the return value.  Returns rtfEOF when there
  are no more tokens. }
function TRTFParser.GetToken : Integer;
var
  p : TRTFFuncPTR;
begin
  GetReadHook (p);
  while true do begin
    Real_RTFGetToken;
    if (assigned(p)) then p();  { give read hook a look at token }
    { Silently discard newlines and carriage returns.  }
    if not ((rtfClass=rtfText) and ((rtfMajor=13) or (rtfmajor=10))) then
      Break;
  end;
  Result:=rtfClass;
end;

{ ---------------------------------------------------------------------
   Install or return a token reader hook.
  ---------------------------------------------------------------------}
procedure TRTFParser.SetReadHook(Hook : TRTFFuncPtr);
begin
  readHook := Hook;
end;

procedure TRTFParser.GetReadHook(Var q : TRTFFuncPtr);
begin
  Q:=readHook;
end;

procedure TRTFParser.UngetToken;
begin
  if (pushedClass >= 0) then      { there's already an ungotten token }
    Error ('cannot unget two tokens');
  if (rtfClass < 0) then
    Error ('no token to unget');
  pushedClass := rtfClass;
  pushedMajor := rtfMajor;
  pushedMinor := rtfMinor;
  pushedParam := rtfParam;
  rtfTextBuf  := pushedTextBuf;
end;

function TRTFParser.PeekToken : Integer;
begin
  Real_RTFGetToken;
  UngetToken;
  Result:=rtfClass;
end;

Procedure TRTFParser.Real_RTFGetToken;
var
  sign,c,c2 : Integer;
begin
  { check for pushed token from RTFUngetToken() }
  if (pushedClass >= 0) then begin
    FrtfClass  := pushedClass;
    FrtfMajor  := pushedMajor;
    FrtfMinor  := pushedMinor;
    FrtfParam  := pushedParam;
    rtfTextBuf := pushedTextBuf;
    rtfTextLen := length (rtfTextBuf);
    pushedClass := -1;
    Exit;
  end;

  { initialize token vars }
  FrtfClass   := rtfUnknown;
  FrtfParam   := rtfNoParam;
  rtfTextBuf := '';
  rtfTextLen := 0;
  FTokenClass := rtfUnknown;

  { get first character, which may be a pushback from previous token }

  if (pushedChar <> EOF) then begin
    c := pushedChar;
    rtfTextBuf:=rtfTextBuf+chr(c);
    inc(rtftextlen);
    pushedChar := EOF;
  end else begin
    c:=GetChar;
    if C=EOF then begin
      FrtfClass := rtfEOF;
      Exit;
    end;
  end;

  if c=ord('{') then begin
    FrtfClass := rtfGroup;
    FrtfMajor := rtfBeginGroup;
    exit;
  end;

  if c=ord('}') then begin
    FrtfClass := RTFGROUP;
    FrtfMajor := rtfEndGroup;
    exit;
  end;

  if c<>ord('\') then begin
    { Two possibilities here:
      1) ASCII 9, effectively like \tab control symbol
      2) literal text char }
    if c=ord(#8) then begin               { ASCII 9 }
      FrtfClass := rtfControl;
      FrtfMajor := rtfSpecialChar;
      FrtfMinor := rtfTab;
    end else begin
      FrtfClass := rtfText;
      FrtfMajor := c;
    end;
    Exit;
  end;

  c:=getchar;
  if (c=EOF) then { early eof, whoops (class is rtfUnknown) }
    Exit;

  if ( not isalpha (c)) then begin
    { Three possibilities here:
     1) hex encoded text char, e.g., \'d5, \'d3
     2) special escaped text char, e.g., \, \;
     3) control symbol, e.g., \_, \-, \|, \<10> }
    if c=ord('''') then begin{ hex char }
      c:=getchar;
      if (c<>EOF) then begin
        c2:=getchar;
        if (c2<>EOF) then begin
          { should do isxdigit check! }
          FrtfClass := rtfText;
          FrtfMajor := HexVal (c) * 16 + HexVal (c2);
          Exit;
        end;
      end;
      { early eof, whoops (class is rtfUnknown) }
      Exit;
    end;
    if pos (chr(c),':{};\')<>0 then begin{ escaped char }
      FrtfClass := rtfText;
      FrtfMajor := c;
      exit;
    end;

    { control symbol }
    Lookup (rtfTextBuf); { sets class, major, minor }
    FTokenClass:=rtfControl;
    exit;
  end;

  { control word }
  while (isalpha (c)) do begin
    c:=GetChar;
    if (c=EOF) then Break;
  end;

  { At this point, the control word is all collected, so the
    major/minor numbers are determined before the parameter
    (if any) is scanned.  There will be one too many characters
    in the buffer, though, so fix up before and restore after
    looking up. }
  if (c<>EOF) then Delete(rtfTextBuf,length(rtfTextbuf),1);
  Lookup (rtfTextBuf);    { sets class, major, minor }
  FTokenClass:=rtfControl;
  if (c <>EOF) then
    rtfTextBuf:=rtfTextBuf+chr(c);

  { Should be looking at first digit of parameter if there
    is one, unless it's negative.  In that case, next char
    is '-', so need to gobble next char, and remember sign. }
  sign := 1;
  if c = ord('-') then begin
    sign := -1;
    c := GetChar;
  end;

  if (c<>EOF) then
    if isdigit (c) then begin
      FrtfParam := 0;
      while (isdigit (c)) do begin  { gobble parameter }
        FrtfParam := FrtfParam * 10 + c - ord('0');
        c:=GetChar;
        if (c=EOF) then Break;
      end;
      FrtfParam:= sign*FrtfParam;
    end;

  { If control symbol delimiter was a blank, gobble it.
   Otherwise the character is first char of next token, so
   push it back for next call.  In either case, delete the
   delimiter from the token buffer. }
  if (c<>EOF) then begin
    if c<>ord (' ') then pushedChar := c;
    Delete (rtfTextBuf,rtfTextLen,1);
    Dec (rtfTextLen);
  end;
end;

function TRTFParser.GetChar : Integer;
var
  c : byte;
begin
  if FStream.read(c,1)<>0 then begin
    if (c and 128)=128 then c:=ord('?');
    Result:=c;
    rtfTextBuf:=rtfTextBuf+chr(c);
    inc(rtfTextLen);
  end else
    Result:=EOF;
end;

{ Synthesize a token by setting the global variables to the
  values supplied.  Typically this is followed with a call
  to RTFRouteToken().
  If param is non-negative, it becomes part of the token text. }
Procedure TRTFParser.SetToken (Aclass, major, minor, param : Integer; text : string);
Begin
  FrtfClass := Aclass;
  FrtfMajor := major;
  FrtfMinor := minor;
  FrtfParam := param;
  if (param=rtfNoParam) then
    rtfTextBuf:=text
  else
    rtfTextBuf:=text+IntTostr(param);
  rtfTextLen:=length(rtfTextBuf);
end;

{ ---------------------------------------------------------------------
       Special destination readers.  They gobble the destination so the
       writer doesn't have to deal with them.  That's wrong for any
       translator that wants to process any of these itself.  In that
       case, these readers should be overridden by installing a different
       destination callback.

       NOTE: The last token read by each of these reader will be the
       destination's terminating '', which will then be the current token.
       That 'End;' token is passed to RTFRouteToken() - the writer has already
       seen the 'Begin' that began the destination group, and may have pushed a
       state; it also needs to know at the end of the group that a state
       should be popped.

       It's important that rtfdata.inc and the control token lookup table list
       as many symbols as possible, because these readers unfortunately
       make strict assumptions about the input they expect, and a token
       of class rtfUnknown will throw them off easily.
 ----------------------------------------------------------------------}


{ Read Begin \fonttbl ... End; destination.  Old font tables don't have
  braces around each table entry; try to adjust for that.}
procedure TRTFParser.ReadFontTbl;
var
  fp : PRTFFont;
  bp : string[rtfbufsiz];
  old : Integer;
begin
  old := -1;

  while true do begin
    GetToken;
    if CheckCM (rtfGroup, rtfEndGroup) then break;
    if (old < 0) then begin { first entry - determine tbl type }
      if CheckCMM (rtfControl, rtfCharAttr, rtfFontNum) then
        old:=1    { no brace }
      else if CheckCM (rtfGroup, rtfBeginGroup) then
        old:= 0   { brace }
      else                        { can't tell! }
      Error ('FTErr - Cannot determine format')
    end;
    if (old=0) then begin { need to find "Begin" here }
      if not CheckCM (rtfGroup, rtfBeginGroup) then
        Error ('FTErr - missing {');
      GetToken;   { yes, skip to next token }
    end;

    new(fp);
    if (fp=nil) then
      Error ('FTErr - cannot allocate font entry');
    fp^.rtfNextFont:= FfontList;
    FfontList:=fp;
    if not CheckCMM (rtfControl, rtfCharAttr, rtfFontNum) then
      Error ('FTErr - missing font number');
    fp^.rtfFNum := rtfParam;

    { Read optionalcommands. Recognize only fontfamily}
    GetToken;
    if not CheckCM (rtfControl, rtfFontFamily) then
      error ('FTErr - missing font family ');
    fp^.rtfFFamily := rtfMinor;

    { Read optional commands/groups. Recognize none at this point..}
    GetToken;
    while (rtfclass=rtfcontrol) or ((rtfclass=rtfgroup) or (rtfclass=rtfunknown)) do begin
      if rtfclass=rtfgroup then SkipGroup;
      GetToken
    end;

    { Read font name }
    bp:='';
    while (rtfclass=rtfText) do begin
      if rtfMajor=ord(';') then Break;
      bp:=bp+chr(rtfMajor);
      GetToken
    end;

    if bp='' then Error ('FTErr - missing font name');
    fp^.rtffname:=bp;

    { Read alternate font}
    if (old=0) then begin { need to see "End;" here }
      GetToken;
      if not CheckCM (rtfGroup, rtfEndGroup) then
        Error ('FTErr - missing }');
    end;
  end;

  RouteToken;     { feed "End;" back to router }
end;


{ The color table entries have color values of -1 if
  the default color should be used for the entry (only
  a semi-colon is given in the definition, no color values).
  There will be a problem if a partial entry (1 or 2 but
  not 3 color values) is given.  The possibility is ignored
  here. }
Procedure TRTFParser.ReadColorTbl;
var
  cp   : PRTFColor;
  cnum : Integer;
Begin
  cnum:=0;
  while true do begin
    GetToken;
    if CheckCM (rtfGroup, rtfEndGroup) then Break;
    new(cp);
    if (cp=nil) then
      Error ('CTErr - cannot allocate color entry');
    cp^.rtfCNum  :=cnum;
    cp^.rtfCRed  :=-1;
    cp^.rtfCGreen:=-1;
    cp^.rtfCBlue :=-1;
    cp^.rtfNextColor := FColorList;
    inc(cnum);
    FcolorList:=cp;
    while true do begin
      if not CheckCM (rtfControl, rtfColorName) then Break;
      case rtfMinor of
        rtfRed:   cp^.rtfCRed   :=rtfParam;
        rtfGreen: cp^.rtfCGreen :=rtfParam;
        rtfBlue:  cp^.rtfCBlue  :=rtfParam;
      end;
      GetToken;
    end;
    if not CheckCM (rtfText, ord(';')) then
       Error ('CTErr - malformed entry');
  end;
  RouteToken;     { feed "End;" back to router }
end;


{ The "Normal" style definition doesn't contain any style number
 (why?), all others do.  Normal style is given style 0. }

Procedure TRTFParser.ReadStyleSheet;
var
  sp          : PRTFStyle;
  sep,sepLast : PRTFStyleElt;
  bp          : string[rtfBufSiz];
begin
  while true do begin
    GetToken;
    if CheckCM (rtfGroup, rtfEndGroup) then Break;
    new (sp);
    if sp=nil then Error ('SSErr - cannot allocate stylesheet entry');
    sp^.rtfSNum := -1;
    sp^.rtfSBasedOn := rtfBasedOnNone;
    sp^.rtfSNextPar := -1;
    sp^.rtfSSEList := nil;
    sepLast:=nil;
    sp^.rtfNextStyle := FstyleList;
    sp^.rtfExpanding := 0;
    FstyleList := sp;
    if not CheckCM (rtfGroup, rtfBeginGroup) then Error ('SSErr - missing {');
    while (GetToken=rtfControl) or (FTokenClass=rtfControl) do begin
      if rtfClass=rtfUnknown then Continue;
      if (CheckMM (rtfParAttr, rtfStyleNum)) then begin
        sp^.rtfSNum:=rtfParam;
        continue;
      end;
      if (CheckMM (rtfStyleAttr, rtfBasedOn)) then begin
        sp^.rtfSBasedOn:=rtfParam;
        continue;
      end;
      if (CheckMM (rtfStyleAttr, rtfNext)) then begin
        sp^.rtfSNextPar:=rtfParam;
        Continue;
      end;
      new(sep);
      if sep=nil then
        Error ('SSErr - cannot allocate style element');
      sep^.rtfSEClass:=rtfClass;
      sep^.rtfSEMajor:=rtfMajor;
      sep^.rtfSEMinor:=rtfMinor;
      sep^.rtfSEParam:=rtfParam;
      sep^.rtfSEText:=rtfTextBuf;
      if sepLast=nil then
        sp^.rtfSSEList:=sep      { first element }
      else                                { add to end }
        sepLast^.rtfNextSE:=sep;
      sep^.rtfNextSE:=nil;
      sepLast:=sep;
    end;
    if sp^.rtfSNextPar=-1 then            { \snext not given }
      sp^.rtfSNextPar:=sp^.rtfSNum;       { next is itself }
    if rtfClass<>rtfText then
      Error ('SSErr - missing style name');
    Bp:='';
    while rtfClass=rtfText do begin
      if rtfMajor=ord(';') then begin
        GetToken;
        Break;
      end;
      bp:=bp+chr(rtfMajor);
      GetToken;
    end;
    if (sp^.rtfSNum < 0) then begin { no style number was specified, (only legal for Normal style) }
      if bp<>'Normal' then
         Error ('SSErr - missing style number');
      sp^.rtfSNum:=0;
    end;
    sp^.rtfSName:=bp;
    if not CheckCM (rtfGroup, rtfEndGroup) then
      Error ('SSErr - missing }');
  end;
  RouteToken;     { feed "End;" back to router }
end;


Procedure TRTFParser.ReadInfoGroup;
Begin
  SkipGroup;
  RouteToken;  { feed "End;" back to router }
end;

Procedure TRTFParser.ReadPictGroup;
Begin
  SkipGroup;
  RouteToken;  { feed "End;" back to router }
end;


{ ----------------------------------------------------------------------
    Routines to return pieces of stylesheet, or font or color tables
  ----------------------------------------------------------------------}
function TRTFParser.GetStyle (num : Integer) : PRTFStyle;
var
  s : PRTFSTyle;
begin
  s:=Fstylelist;
  if num<>1 then
    while s<>nil do begin
      if (s^.rtfSNum=num) then break;
      s:=s^.rtfNextStyle;
    end;
  Result:=s;              { NULL if not found }
end;

function TRTFParser.GetFont (num : Integer) : PRTFFont;
var
  f :PRTFFont;
begin
  f:=FfontList;
  if num<>-1 then
    while f<>nil do begin
      if f^.rtfFNum=num then break;
      f:=f^.rtfNextFont;
    end;
  Result:=f; { NULL if not found }
end;

function TRTFParser.GetColor (num : Integer) : PRTFColor;
var
  c : PRTFColor;
begin
  c:=Fcolorlist;
  if (num<>-1) then
    while c<>nil do begin
      if c^.rtfCNum=num then break;
      c:=c^.rtfNextColor;
    end;
  Result:=c;              { NULL if not found }
End;

{ ---------------------------------------------------------------------
       Expand style n, if there is such a style.
  ---------------------------------------------------------------------}

procedure TRTFParser.ExpandStyle (n : Integer);
var
  s  : PRTFStyle;
  se : PRTFStyleElt;
begin
  if n=-1 then Exit;
  s:=GetStyle (n);
  if s=nil then Exit;

  if (s^.rtfExpanding<>0) then
    Error ('Style expansion loop, style '+inttostr(n));
  s^.rtfExpanding:=1;     { set expansion flag for loop detection }
{
        Expand "based-on" style.  This is done by synthesizing
        the token that the writer needs to see in order to trigger
        another style expansion, and feeding to token back through
        the router so the writer sees it.
}
  SetToken (rtfControl, rtfParAttr, rtfStyleNum, s^.rtfSBasedOn, '\s');
  RouteToken;

{
        Now route the tokens unique to this style.  RTFSetToken()
        isn't used because it would add the param value to the end
        of the token text, which already has it in.
}
  se:=s^.rtfSSEList;
  while se<>nil do begin
    FrtfClass:=se^.rtfSEClass;
    FrtfMajor:=se^.rtfSEMajor;
    FrtfMinor:=se^.rtfSEMinor;
    FrtfParam:=se^.rtfSEParam;
    rtfTextBuf:=se^.rtfSEText;
    rtfTextLen:=length (rtfTextBuf);
    RouteToken;
    se:=se^.rtfNextSE
  end;
  s^.rtfExpanding:=0;     { done - clear expansion flag }
End;

function TRTFParser.GetRtfText: string;
begin
  SetString(Result, @rtfTextBuf[1], rtfTextLen);
end;

{ ---------------------------------------------------------------------
       Initialize lookup table hash values.
       Only need to do this the first time it's called.
  ---------------------------------------------------------------------}

Procedure TRTFParser.LookupInit;
var
  count : Integer;
begin
  count:=0;
  while rtfkey[count].rtfKStr<>'' do begin
    rtfkey[count].rtfKHash:=Hash (rtfkey[count].rtfKStr);
    inc(count)
  end;
end;

{ ---------------------------------------------------------------------
       Determine major and minor number of control token.  If it's
       not found, the class turns into rtfUnknown.
  ---------------------------------------------------------------------}
procedure TRTFParser.Lookup (S : String);
var
  thehash,rp : Integer;
begin
  delete(s,1,1);                  { skip over the leading \ character }
  thehash:=Hash (s);
  rp:=0;
  while rtfkey[rp].rtfKstr<>'' do begin
    if (thehash=rtfkey[rp].rtfKHash) and (s=rtfkey[rp].rtfKStr) then begin
      FrtfClass:=rtfControl;
      FrtfMajor:=rtfkey[rp].rtfKMajor;
      FrtfMinor:=rtfkey[rp].rtfKMinor;
      exit;
    end;
    inc(rp);
  end;
  FrtfClass:=rtfUnknown;
End;

procedure TRTFParser.Error (msg : String);
{ Call errorhandler }
begin
  if assigned(onrtferror) then onrtferror(msg);
end;
{ ---------------------------------------------------------------------
       Token comparison routines
  ---------------------------------------------------------------------}

function TRTFParser.CheckCM (Aclass, major: Integer) : Boolean;
begin
  Result:=(rtfClass=Aclass) and (rtfMajor=major);
end;

function TRTFParser.CheckCMM (Aclass, major, minor : Integer) : Boolean;
begin
  Result:=(rtfClass=Aclass) and ((rtfMajor=major) and (rtfMinor=minor));
end;

function TRTFParser.CheckMM (major, minor : Integer) : Boolean;
begin
  Result:=(rtfMajor=major) and (rtfMinor=minor);
end;

procedure TRTFParser.SetStream (Astream : TStream);
begin
  FStream:=Astream;
end;

end.
