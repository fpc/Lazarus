{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Parsing fpc message files.
    FPC prints message IDs with -vq
}
(*
    For example:
general_t_compilername=01000_T_Compiler: $1
% When the \var{-vt} switch is used, this line tells you what compiler
% is used.

<part>_<type>_<txtidentifier>=<id>_<idtype>_<message with plcaeholders>

*)
unit CodeToolsFPCMsgs;

{$mode objfpc}{$H+}

{off $DEFINE VerboseFPCMsgFile}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  LazUTF8Classes,
  // Codetools
  FileProcs;

type
  TfmiSpecialItem = (
    fmisiNone,
    fmisiFatal,
    fmisiError,
    fmisiWarning,
    fmisiNote,
    fmisiHint
    );
  TfmiSpecialItems = set of TfmiSpecialItem;

  { TFPCMsgItem }

  TFPCMsgItem = class
  public
    Part: string; // e.g. 'general', 'unit', 'link'
    Typ: string; // e.g. 'f','e','w','n','h','i','l','u','t','c','d','x','o'
    TxtIdentifier: string; // identifier
    ID: integer; // positive number
    ShownTyp: string; // e.g. shown Typ, can be different from Typ
    Pattern: string; // Text with placeholders $1 .. $9
    PatternEndSpace: string;
    Comment: string; // multi line

    Index: integer; // index in list
    function GetName(WithID: boolean = true): string; // part_typ_txtidentifier=id
    function PatternFits(aMsg: string): integer; // >=0 fits
    function GetTrimmedComment(NoLineBreaks, NoLatex: boolean): string;
  end;

  { TFPCMsgFile }

  TFPCMsgFile = class
  private
    fSpecialItems: array[TfmiSpecialItem] of TFPCMsgItem;
    FItems: TFPList; // list of TFPCMsgItem
    fSortedForID: TAVLTree; // tree of TFPCMsgItem sorted for ID
    fItemById: array of TFPCMsgItem;
    fNodeMgr: TAVLTreeNodeMemManager;
    function GetItems(Index: integer): TFPCMsgItem;
    procedure CreateArray;
    function GetSpecialItems(Index: TfmiSpecialItem): TFPCMsgItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromList(List: TStrings); virtual;
    procedure LoadFromText(s: string); virtual;
    procedure Clear; virtual;
    function Count: integer;
    property Items[Index: integer]: TFPCMsgItem read GetItems; default;
    function FindWithID(ID: integer): TFPCMsgItem;
    function FindWithMessage(Msg: string): TFPCMsgItem;
    function GetMsgText(Item: TFPCMsgItem): string; // prepends msg type (e.g. Error:)
    function PatternFits(Item: TFPCMsgItem; aMsg: string): integer; // >=0 fits
    property SpecialItems[Index: TfmiSpecialItem]: TFPCMsgItem read GetSpecialItems;
    function MsgTypToSpecialItem(const Typ: string): TFPCMsgItem;
  end;

type
  TFPCMsgRange = record
    StartPos: integer;
    EndPos: integer;
  end;
  PFPCMsgRange = ^TFPCMsgRange;

  { TFPCMsgRanges }

  TFPCMsgRanges = class
  private
    FCount: integer;
    FCapacity: integer;
  public
    Ranges: PFPCMsgRange;
    property Count: integer read FCount;
    property Capacity: integer read FCapacity;
    procedure Add(StartPos, EndPos: integer);
    procedure Clear(FreeMemory: boolean = false);
    destructor Destroy; override;
  end;

type
  TFPCMsgFileToEncoding = record
    Filename: String;
    Encoding: String;
  end;
const
  FPCMsgFileToEncoding: array[1..21] of TFPCMsgFileToEncoding = (
    (Filename: 'errorct.msg';  Encoding: 'CP1252'), // Catalan
    (Filename: 'errord.msg';   Encoding: 'CP437'), // German
    (Filename: 'errorda.msg';  Encoding: 'UTF-8'), // Danish
    (Filename: 'errordu.msg';  Encoding: 'UTF-8'), // German
    (Filename: 'errore.msg';   Encoding: 'UTF-8'), // English
    (Filename: 'errores.msg';  Encoding: 'CP1252'), // Spanish
    (Filename: 'errorf.msg';   Encoding: 'CP850'), // French
    (Filename: 'errorfi.msg';  Encoding: 'ISO-8859-1'), // French
    (Filename: 'errorhe.msg';  Encoding: 'CP1255'), // Hebrew
    (Filename: 'errorheu.msg'; Encoding: 'UTF-8'), // Hebrew
    (Filename: 'errorid.msg';  Encoding: 'UTF-8'), // Indonesian
    (Filename: 'erroriu.msg';  Encoding: 'CP1252'), // Italian
    (Filename: 'errorn.msg';   Encoding: 'CP850'), // Dutch
    (Filename: 'errorpl.msg';  Encoding: 'CP852'), // Polish
    (Filename: 'errorpli.msg'; Encoding: 'ISO-8859-2'), // Polish
    (Filename: 'errorpt.msg';  Encoding: 'CP850'), // Portuguese
    (Filename: 'errorptu.msg'; Encoding: 'UTF-8'), // Portuguese
    (Filename: 'errorr.msg';   Encoding: 'CP866'), // Russian
    (Filename: 'errorru.msg';  Encoding: 'UTF-8'), // Russian
    (Filename: 'errorues.msg'; Encoding: 'UTF-8'), // Spanish
    (Filename: 'errorcn.msg';  Encoding: 'CP936') // Chinese
  );

function CompareFPCMsgId(item1, item2: Pointer): integer;
function CompareIDWithFPCMsgId(PtrID, Item: Pointer): integer;
procedure ExtractFPCMsgParameters(const Mask, Txt: string; var Ranges: TFPCMsgRanges);
function GetDefaultFPCErrorMsgFileEncoding(Filename: string): string;

function dbgs(i: TfmiSpecialItem): string; overload;

implementation

function CompareFPCMsgId(item1, item2: Pointer): integer;
var
  Msg1: TFPCMsgItem absolute item1;
  Msg2: TFPCMsgItem absolute item2;
begin
  if Msg1.ID<Msg2.ID then
    exit(-1)
  else if Msg1.ID>Msg2.ID then
    exit(1)
  else
    exit(0);
end;

function CompareIDWithFPCMsgId(PtrID, Item: Pointer): integer;
var
  Msg: TFPCMsgItem absolute Item;
  ID: LongInt;
begin
  ID:=PInteger(PtrID)^;
  if ID<Msg.ID then
    exit(-1)
  else if ID>Msg.ID then
    exit(1)
  else
    exit(0);
end;

procedure ExtractFPCMsgParameters(const Mask, Txt: string;
  var Ranges: TFPCMsgRanges);
{ Examples:
   Mask: bla$1blo
   Txt: blatestblo
   Result:=['test']
}

  function FindEndOfNextMatch(MaskStartPos, MaskEndPos, TxtStartPos: PChar): PChar;
  var
    TxtPos: PChar;
    MaskPos: PChar;
  begin
    while TxtStartPos^<>#0 do begin
      TxtPos:=TxtStartPos;
      MaskPos:=MaskStartPos;
      while (MaskPos<MaskEndPos) and (MaskPos^=TxtPos^) do begin
        inc(MaskPos);
        inc(TxtPos);
      end;
      if MaskPos=MaskEndPos then begin
        Result:=TxtPos;
        exit;
      end;
      inc(TxtStartPos);
    end;
    Result:=nil;
  end;

var
  BaseMaskPos: PChar;
  BaseTxtPos: PChar;
  MaskPos: PChar;
  TxtPos: PChar;
  MaskStartPos: PChar;
  TxtEndPos: PChar;
begin
  if Ranges=nil then
    Ranges:=TFPCMsgRanges.Create;
  Ranges.Clear();
  if Mask='' then exit;
  BaseMaskPos:=PChar(Mask);
  if Txt='' then
    BaseTxtPos:=#0
  else
    BaseTxtPos:=PChar(Txt);

  MaskPos:=BaseMaskPos;
  TxtPos:=BaseTxtPos;
  while (MaskPos^=TxtPos^) do begin
    if MaskPos^=#0 then exit;
    if (MaskPos^='$') and (MaskPos[1]<>'$') then break;
    inc(MaskPos);
    inc(TxtPos);
  end;
  while MaskPos^='$' do begin
    // skip variable in mask
    inc(MaskPos);
    while MaskPos^ in ['0'..'9','A'..'Z','a'..'z','_'] do inc(MaskPos);
    // get next pattern in mask
    MaskStartPos:=MaskPos;
    if MaskStartPos^=#0 then begin
      // variable at end of mask
      Ranges.Add(TxtPos-BaseTxtPos,length(Txt)+1);
      exit;
    end;
    while (MaskPos^<>#0) and (MaskPos^<>'$') do inc(MaskPos);
    // search pattern in txt
    TxtEndPos:=FindEndOfNextMatch(MaskStartPos,MaskPos,TxtPos);
    if TxtEndPos=nil then exit;
    Ranges.Add(TxtPos-BaseTxtPos,TxtEndPos-BaseTxtPos-(MaskPos-MaskStartPos));
    TxtPos:=TxtEndPos;
  end;
end;

function GetDefaultFPCErrorMsgFileEncoding(Filename: string): string;
var
  i: Integer;
begin
  Filename:=ExtractFileName(Filename);
  for i:=low(FPCMsgFileToEncoding) to high(FPCMsgFileToEncoding) do
    if FPCMsgFileToEncoding[i].Filename=Filename then
      exit(FPCMsgFileToEncoding[i].Encoding);
  Result:='';
end;

function dbgs(i: TfmiSpecialItem): string;
begin
  case i of
  fmisiFatal: Result:='fatal';
  fmisiError: Result:='error';
  fmisiWarning: Result:='warning';
  fmisiNote: Result:='note';
  fmisiHint: Result:='hint';
  else Result:='?';
  end;
end;

{ TFPCMsgItem }

function TFPCMsgItem.GetName(WithID: boolean): string;
// part_typ_txtidentifier=id
begin
  Result:=Part+'_';
  if Typ<>'' then Result:=Result+Typ+'_';
  Result:=Result+TxtIdentifier;
  if WithID then
    Result:=Result+'='+IntToStr(ID);
end;

function TFPCMsgItem.PatternFits(aMsg: string): integer;
var
  PatStartPos: PChar;
  PatEndPos: PChar;
  MsgFitPos: PChar;
  MatchLen: Integer;
  MsgPos: PChar;
  PatPos: PChar;
  MsgStartPos: PChar;
  PatLen: Integer;
begin
  Result:=-1;
  // Pattern is for example "$1 lines compiled, $2 sec$3"
  if (aMsg='') or (Pattern='') then exit;

  // aMsg can start with a filename => hard to tell where the message starts
  // the Pattern is always at the end of aMsg => quick check the end
  PatLen:=length(Pattern);
  if (PatLen>=2)
  and ((Pattern[PatLen-1]<>'$') or (not (Pattern[PatLen] in ['0'..'9'])))
  then begin
    // the pattern does not have a placeholder at the end
    // => the tail must be the pattern => check tail
    PatStartPos:=PChar(Pattern);
    PatEndPos:=@Pattern[PatLen];
    MsgPos:=@aMsg[length(aMsg)];
    MsgStartPos:=PChar(aMsg);
    while (PatEndPos^=MsgPos^) do begin
      if PatEndPos=PatStartPos then begin
        // pattern has no placeholders and whole pattern fits
        Result:=PatLen;
        exit;
      end;
      dec(PatEndPos);
      if (PatEndPos^ in ['0'..'9']) and (PatEndPos[-1]='$') then begin
        // pattern behind last placeholder fits
        // => a full check is needed
        break;
      end;
      if MsgPos=MsgStartPos then begin
        // pattern does not fit
        exit(-1);
      end;
      dec(MsgPos);
    end;
  end;

  PatEndPos:=PChar(Pattern);
  MsgFitPos:=PChar(aMsg);
  MatchLen:=0;
  repeat
    PatStartPos:=PatEndPos;
    // get next pattern between placeholders
    repeat
      if PatEndPos^=#0 then break;
      if (PatEndPos^='$') and (PatEndPos[1] in ['0'..'9']) then break;
      inc(PatEndPos);
    until false;
    if PatEndPos<>PatStartPos then begin
      // search pattern in Pattern
      repeat
        MsgPos:=MsgFitPos;
        PatPos:=PatStartPos;
        while (MsgPos^=PatPos^) and (PatPos<PatEndPos) do begin
          inc(MsgPos);
          inc(PatPos);
        end;
        if PatPos=PatEndPos then
          break;
        // does not fit => check next
        inc(MsgFitPos);
      until MsgFitPos^=#0;
      if PatPos<PatEndPos then
        exit(-1); // pattern not found => does not fit
      inc(MatchLen,PatEndPos-PatStartPos);
      // pattern fits, search the rest of the patterns behind this position
      MsgFitPos:=MsgPos;
    end;
    if PatEndPos^=#0 then begin
      // whole pattern fits
      Result:=MatchLen;
      exit;
    end;
    // skip placeholder $d
    inc(PatEndPos,2);
  until false;
end;

function TFPCMsgItem.GetTrimmedComment(NoLineBreaks, NoLatex: boolean): string;
var
  i: Integer;
  StartPos: Integer;
begin
  Result:=Comment;
  if NoLatex then begin
    // remove tags
    i:=1;
    while i<length(Result) do begin
      if Result[i]='\' then begin
        StartPos:=i;
        inc(i);
        if Result[i] in ['a'..'z','A'..'Z'] then begin
          // \tag
          while (i<=length(Result))
          and (Result[i] in ['a'..'z','A'..'Z','0'..'9','_']) do
            inc(i);
          System.Delete(Result,StartPos,i-StartPos);
          i:=StartPos;
        end else begin
          // special character
          System.Delete(Result,StartPos,1);
        end;
      end else if Result[i] in ['{','}'] then begin
        System.Delete(Result,i,1);
      end else begin
        inc(i);
      end;
    end;
  end;
  for i:=length(Result) downto 1 do begin
    if NoLineBreaks and (Result[i] in [#10,#13]) then
      Result[i]:=' '
    else if Result[i]=#9 then
      Result[i]:=' ';
    if Result[i]=' ' then begin
      if (i=1) or (i=length(Result)) or (Result[i+1] in [' ',#10,#13]) then
        system.Delete(Result,i,1);
    end;
  end;
end;

{ TFPCMsgFile }

function TFPCMsgFile.GetItems(Index: integer): TFPCMsgItem;
begin
  Result:=TFPCMsgItem(FItems[Index]);
end;

procedure TFPCMsgFile.CreateArray;
var
  MaxID: Integer;
  i: Integer;
  Item: TFPCMsgItem;
  MinID: Integer;
begin
  //debugln(['TFPCMsgFile.CreateArray START']);
  SetLength(fItemById,0);
  if fSortedForID.Count=0 then
    exit;
  Item:=TFPCMsgItem(fSortedForID.FindLowest.Data);
  MinID:=Item.ID;
  if MinID<0 then begin
    debugln(['TFPCMsgFile.CreateArray WARNING: MinID ',MinID,' too low: ',Item.Pattern]);
    exit;
  end;
  Item:=TFPCMsgItem(fSortedForID.FindHighest.Data);
  MaxID:=Item.ID;
  if MaxID>100000 then begin
    debugln(['TFPCMsgFile.CreateArray WARNING: MaxID ',MaxID,' too high: ',Item.Pattern]);
    exit;
  end;
  //debugln(['TFPCMsgFile.CreateArray Max=',MaxID]);
  SetLength(fItemById,MaxID+1);
  for i:=0 to length(fItemById)-1 do fItemById[i]:=nil;
  for i:=0 to FItems.Count-1 do begin
    Item:=TFPCMsgItem(FItems[i]);
    //debugln(['TFPCMsgFile.CreateArray ',Item.ID,' ',copy(Item.Pattern,1,20),'..',copy(Item.Pattern,length(Item.Pattern)-19,20)]);
    fItemById[Item.ID]:=Item;
  end;
end;

function TFPCMsgFile.GetSpecialItems(Index: TfmiSpecialItem): TFPCMsgItem;
begin
  Result:=fSpecialItems[Index];
end;

constructor TFPCMsgFile.Create;
begin
  inherited Create;
  FItems:=TFPList.Create;
  fSortedForID:=TAVLTree.Create(@CompareFPCMsgId);
  fNodeMgr:=TAVLTreeNodeMemManager.Create;
  fSortedForID.SetNodeManager(fNodeMgr);
end;

destructor TFPCMsgFile.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(fSortedForID);
  FreeAndNil(fNodeMgr);
  inherited Destroy;
end;

procedure TFPCMsgFile.LoadFromFile(const Filename: string);
var
  sl: TStringListUTF8;
begin
  {$IFDEF VerboseFPCMsgFile}
  debugln(['TFPCMsgFile.LoadFromFile START ',Filename]);
  {$ENDIF}
  sl:=TStringListUTF8.Create;
  try
    sl.LoadFromFile(Filename);
    LoadFromList(sl);
  finally
    sl.Free;
  end;
end;

procedure TFPCMsgFile.LoadFromList(List: TStrings);

  function ReadTilChar(var p: PChar; EndChar: char; out s: string): boolean;
  var
    c: Char;
    StartPos: PChar;
  begin
    StartPos:=p;
    repeat
      c:=p^;
      if c=#0 then exit(false);
      if c=EndChar then begin
        break;
      end;
      inc(p);
    until false;
    if p=StartPos then exit(false);
    SetLength(s,p-StartPos);
    System.Move(StartPos^,s[1],length(s));
    inc(p);
    Result:=true;
  end;

  function ReadItem(var Line: integer; const s: string): TFPCMsgItem;
  // <part>_<typ>_<txtidentifier>=<id>_<idtype>_<message with placeholders>
  // option and wpo are different:
  //   <part>_<txtidentifier>=<id>_<idtype>_<message with placeholders>
  // and
  //   <part>_<txtidentifier>=<id>_[<multi line message with placeholders>
  //      ...]
  //
  var
    p: PChar;
    Part: string;
    Typ: string;
    TxtID: string;
    ShownTyp: string;
    IDStr: string;
    ID: LongInt;
    Msg: string;
    h: string;
    i: Integer;
    MsgEndSpace: String;
  begin
    Result:=nil;
    p:=PChar(s);
    if not ReadTilChar(p,'_',Part) then begin
      {$IFDEF VerboseFPCMsgFile}
      debugln(['TFPCMsgFile.LoadFromList invalid <part>, line ',Line,': "',s,'"']);
      {$ENDIF}
      exit;
    end;
    if (Part='option') or (Part='wpo') then
      Typ:=''
    else if not ReadTilChar(p,'_',Typ) then begin
      {$IFDEF VerboseFPCMsgFile}
      debugln(['TFPCMsgFile.LoadFromList invalid <type>, line ',Line,': "',s,'"']);
      {$ENDIF}
      exit;
    end else if (length(Typ)<>1)
      or (not (Typ[1] in ['f','e','w','n','h','i','l','u','t','c','d','x','o']))
    then begin
      {$IFDEF VerboseFPCMsgFile}
      debugln(['TFPCMsgFile.LoadFromList invalid <type>, line ',Line,': "',s,'"']);
      {$ENDIF}
      exit;
    end;
    if not ReadTilChar(p,'=',TxtID) then begin
      {$IFDEF VerboseFPCMsgFile}
      debugln(['TFPCMsgFile.LoadFromList invalid <textidentifier>, line ',Line,': "',s,'"']);
      {$ENDIF}
      exit;
    end;
    if not ReadTilChar(p,'_',IDStr) then begin
      {$IFDEF VerboseFPCMsgFile}
      debugln(['TFPCMsgFile.LoadFromList invalid id, line ',Line,': "',s,'"']);
      {$ENDIF}
      exit;
    end;
    ID:=StrToIntDef(IDStr,-1);
    if ID<0 then begin
      {$IFDEF VerboseFPCMsgFile}
      debugln(['TFPCMsgFile.LoadFromList invalid id, line ',Line,': "',s,'"']);
      {$ENDIF}
      exit;
    end;
    ShownTyp:='';
    if p<>'[' then begin
      if not ReadTilChar(p,'_',ShownTyp) then begin
        {$IFDEF VerboseFPCMsgFile}
        debugln(['TFPCMsgFile.LoadFromList invalid urgency, line ',Line,': "',s,'"']);
        {$ENDIF}
        exit;
      end;
      Msg:=p;
    end else begin
      // multi line message
      Msg:='';
      repeat
        inc(Line);
        if Line>=List.Count then exit;
        h:=List[Line];
        //debugln(['ReadItem ID=',ID,' h=',h]);
        if (h<>'') and (h[1]=']') then break;
        Msg:=Msg+h+LineEnding;
      until false;
    end;

    i:=length(Msg);
    while (i>=1) and (Msg[i] in [' ',#9,#10,#13]) do dec(i);
    if i<length(Msg) then begin
      MsgEndSpace:=copy(Msg,i+1,length(Msg));
      System.Delete(Msg,i+1,length(Msg));
    end else
      MsgEndSpace:='';

    Result:=TFPCMsgItem.Create;
    Result.Part:=Part;
    Result.Typ:=Typ;
    Result.TxtIdentifier:=TxtID;
    Result.ID:=ID;
    Result.ShownTyp:=ShownTyp;
    Result.Pattern:=Msg;
    Result.PatternEndSpace:=MsgEndSpace;
    //debugln(['ReadItem Part=',Part,' Typ=',Typ,' TxtID=',TxtID,' ID=',ID,' IdTyp=',ShownTyp,' Msg="',copy(Result.Pattern,1,20),'"']);
  end;

var
  Line: Integer;
  s: string;
  Item: TFPCMsgItem;
begin
  //debugln(['TFPCMsgFile.LoadFromList START']);
  Clear;
  Line:=0;
  Item:=nil;
  while Line<List.Count do begin
    s:=List[Line];
    if s='' then begin
      // empty line
      Item:=nil;
    end else if s[1]='#' then begin
      // comment
    end else if s[1]='%' then begin
      // item comment
      if Item<>nil then begin
        if Item.Comment<>'' then
          Item.Comment:=Item.Comment+LineEnding;
        Item.Comment:=Item.Comment+copy(s,2,length(s));
      end;
    end else begin
      Item:=ReadItem(Line,s);
      if Item<>nil then begin
        //debugln(['TFPCMsgFile.LoadFromList ',Item.ID,' ',Item.Pattern]);
        Item.Index:=FItems.Count;
        FItems.Add(Item);
        fSortedForID.Add(Item);
      end;
    end;
    inc(Line);
  end;
  CreateArray;
  fSpecialItems[fmisiFatal]:=FindWithID(1012);
  fSpecialItems[fmisiError]:=FindWithID(1013);
  fSpecialItems[fmisiWarning]:=FindWithID(1014);
  fSpecialItems[fmisiNote]:=FindWithID(1015);
  fSpecialItems[fmisiHint]:=FindWithID(1016);
end;

procedure TFPCMsgFile.LoadFromText(s: string);
var
  sl: TStringList;
begin
  //debugln(['TFPCMsgFile.LoadFromText START']);
  sl:=TStringList.Create;
  try
    sl.Text:=s;
    LoadFromList(sl);
  finally
    sl.Free;
  end;
end;

procedure TFPCMsgFile.Clear;
var
  i: Integer;
  s: TfmiSpecialItem;
begin
  for s:=Low(fSpecialItems) to high(fSpecialItems) do
    fSpecialItems[s]:=nil;
  SetLength(fItemById,0);
  fSortedForID.Clear;
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
end;

function TFPCMsgFile.Count: integer;
begin
  Result:=FItems.Count;
end;

function TFPCMsgFile.FindWithID(ID: integer): TFPCMsgItem;
var
  Node: TAVLTreeNode;
begin
  //debugln(['TFPCMsgFile.FindWithID ',ID,' Max=',length(fItemById)]);
  if (ID>=0) and (ID<length(fItemById)) then begin
    Result:=fItemById[ID];
    exit;
  end;
  Node:=fSortedForID.FindKey(@ID,@CompareIDWithFPCMsgId);
  if Node<>nil then
    Result:=TFPCMsgItem(Node.Data)
  else
    Result:=nil;
end;

function TFPCMsgFile.FindWithMessage(Msg: string): TFPCMsgItem;
var
  MsgID: Integer;
  Item: TFPCMsgItem;
  i: Integer;
  p: PChar;
  BestMatchLen: Integer;
  MatchLen: Integer;
begin
  Result:=nil;
  if Msg='' then exit;
  Msg:=Trim(Msg);
  if Msg='' then exit;
  p:=PChar(Msg);

  // skip time [0.000]
  if (p^='[') and (p[1] in ['0'..'9']) then begin
    inc(p,2);
    while p^ in ['0'..'9','.'] do inc(p);
    if p^<>']' then exit; // not a fpc message
    inc(p);
    while p^ in [' '] do inc(p);
  end;

  // read message ID (000)
  MsgID:=0;
  if (p^='(') and (p[1] in ['0'..'9']) then begin
    inc(p);
    while p^ in ['0'..'9','.'] do begin
      if MsgID>1000000 then exit; // not a fpc message
      MsgID:=MsgID*10+ord(p^)-ord('0');
      inc(p);
    end;
    if p^<>')' then exit; // not a fpc message
    inc(p);
    while p^ in [' '] do inc(p);
    Result:=FindWithID(MsgID);
    exit;
  end;

  // search a message pattern that fits the Msg
  BestMatchLen:=-1;
  for i:=0 to Count-1 do begin
    Item:=Items[i];
    if Item.Pattern='' then continue;
    MatchLen:=PatternFits(Item,Msg);
    if MatchLen>BestMatchLen then begin
      BestMatchLen:=MatchLen;
      Result:=Item;
    end;
  end;
end;

function TFPCMsgFile.GetMsgText(Item: TFPCMsgItem): string;
var
  si: TFPCMsgItem;
begin
  if Item=nil then exit('');
  Result:=Item.Pattern;
  si:=MsgTypToSpecialItem(Item.Typ);
  if si<>nil then
    Result:=si.Pattern+' '+Result;
end;

function TFPCMsgFile.PatternFits(Item: TFPCMsgItem; aMsg: string): integer;
var
  si: TFPCMsgItem;
begin
  Result:=Item.PatternFits(aMsg);
  if Result<0 then exit;
  // some messages have two types
  // => check typ
  si:=MsgTypToSpecialItem(Item.Typ);
  if si<>nil then begin
    if System.Pos(si.Pattern,aMsg)>0 then
      inc(Result,length(si.Pattern));
  end;
end;

function TFPCMsgFile.MsgTypToSpecialItem(const Typ: string): TFPCMsgItem;
begin
  Result:=nil;
  if length(Typ)<>1 then exit;
  case Typ[1] of
  'f': Result:=fSpecialItems[fmisiFatal];
  'e': Result:=fSpecialItems[fmisiError];
  'w': Result:=fSpecialItems[fmisiWarning];
  'n': Result:=fSpecialItems[fmisiNote];
  'h': Result:=fSpecialItems[fmisiHint];
  end;
end;

{ TFPCMsgRanges }

procedure TFPCMsgRanges.Add(StartPos, EndPos: integer);
begin
  if Count=Capacity then begin
    if Capacity<8 then
      fCapacity:=8
    else
      fCapacity:=Capacity*2;
    ReAllocMem(Ranges,Capacity*SizeOf(TFPCMsgRange));
  end;
  Ranges[FCount].StartPos:=StartPos;
  Ranges[FCount].EndPos:=EndPos;
  inc(FCount);
end;

procedure TFPCMsgRanges.Clear(FreeMemory: boolean);
begin
  FCount:=0;
  if FreeMemory then begin
    ReAllocMem(Ranges,0);
    FCapacity:=0;
  end;
end;

destructor TFPCMsgRanges.Destroy;
begin
  Clear(true);
  inherited Destroy;
end;

end.

