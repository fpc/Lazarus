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

 This file is based upon the translations.pas file by Mattias Gaertner
 Author: Bart Broersma
 Year: 2011

  Abstract:
    Methods and classes for loading and checking validity of po-files.

 Note:
   Most references to unneeded methods/functions/procedures are commented out,
   if we later need them, we can easily uncomment the relevant parts
   * For the moment I left out all character encoding stuff: all the relevant
     strings I need to investigate can be investigated without knowing the encoding
     If a program needs to know the encoding, it can read the CharSet property of TSimplePoFile
   * Change the implementation of ReadPoText to use Strings instead of PChars, this resulted in
     a speed-up with a factor 20
     (ifdef-ed the old method)
   * Added LineNr to TPoFileItem

}


{ $define DebugSimplePoFiles}
{ $define ReadPoTextPChar}  //define this to use the old ReadPoText method which uses PChars


unit SimplePoFiles;

{$mode objfpc}{$H+}{$INLINE ON}
{ $include include/lcl_defines.inc}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, StringHashList
  {, LConvEncoding}
  //{$IFDEF UNIX}{$IFNDEF DisableCWString}, cwstring{$ENDIF}{$ENDIF}
  ;

{
type
  TStringsType = (stLrt, stRst);
  TTranslateUnitResult = (turOK, turNoLang, turNoFBLang, turEmptyParam);
}

type
  { TPOFileItem }

  TPOFileItem = class
  public
    LineNr: Integer;
    Tag: Integer;
    Comments: string;
    Identifier: string;
    Original: string;
    Translation: string;
    Flags: string;
    PreviousID: string;
    Context: string;
    constructor Create(const TheIdentifier, TheOriginal, TheTranslated: string);
    procedure ModifyFlag(const AFlag: string; Check: boolean);
  end;

  { TSimplePOFile }

  TSimplePOFile = class
  protected
    FItems: TFPList;// list of TPOFileItem
    FIdentifierToItem: TStringHashList;
    //FIdentVarToItem: TStringHashList;
    FOriginalToItem: TStringHashList;
    FCharSet: String;
    FHeader: TPOFileItem;
    FAllEntries: boolean;
    FTag: Integer;
    //FModified: boolean;
    FHelperList: TStringList;
    FModuleList: TStringList;
    //procedure RemoveTaggedItems(aTag: Integer);
    //procedure RemoveUntaggedModules;
    function GetCount: Integer;
    procedure SetCharSet(const AValue: String);
    {$ifdef ReadPoTextPChar}
    procedure ReadPOText(const Txt: string);
    {$else}
    procedure ReadPOText(AStream: TStream);
    {$endif ReadPoTextPChar}
    function GetPoItem(Index: Integer): TPoFileItem;
  protected
    property Items: TFPList read FItems;
  public
    property OriginalList: TStringHashList read FOriginalToItem;
  public
    constructor Create(const AFilename: String; const Full: Boolean = True);
    constructor Create(AStream: TStream; const Full: Boolean = True);
    destructor Destroy; override;
    procedure Add(const Identifier, OriginalValue, TranslatedValue, Comments,
                        Context, Flags, PreviousID: string; LineNr: Integer);
    //function Translate(const Identifier, OriginalValue: String): String;
    procedure Report;
    procedure Report(StartIndex, StopIndex: Integer; const DisplayHeader: Boolean);
    procedure Report(Log: TStrings; StartIndex, StopIndex: Integer; const DisplayHeader: Boolean);
    procedure CreateHeader;
    //procedure UpdateStrings(InputLines:TStrings; SType: TStringsType);
    //procedure SaveToFile(const AFilename: string);
    //procedure UpdateItem(const Identifier: string; Original: string);
    //procedure UpdateTranslation(BasePOFile: TSimplePOFile);
    //procedure ClearModuleList;
    //procedure AddToModuleList(Identifier: string);
    //procedure UntagAll;

    function FindPoItem(const Identifier: String): TPoFileItem;
    function OriginalToItem(Data: String): TPoFileItem;

    property CharSet: String read FCharSet;
    property Tag: integer read FTag write FTag;
    //property Modified: boolean read FModified;
    property PoItems[Index: Integer]: TPoFileItem read GetPoItem;
    property Count: Integer read GetCount;

  end;

  EPOFileError = class(Exception)
  public
    ResFileName: string;
    POFileName: string;
  end;

var
  SystemCharSetIsUTF8: Boolean = true;// the LCL interfaces expect UTF-8 as default
    // if you don't use UTF-8, install a proper widestring manager and set this
    // to false.


// translate resource strings for one unit
function UTF8ToSystemCharSet(const s: string): string; inline;

//function UpdatePoFile(Files: TStrings; const POFilename: string): boolean;

const
  tgHasDup = $01;

implementation

{$ifdef DebugSimplePoFiles}
var
  T0, T1: DWord; function GetTickCount: DWord;

var
  HH, MM, SS, MS: Word;
begin
  DecodeTime(Now, HH, MM, SS, MS);
  Result := DWord(MS) + (DWord(SS) * 1000) + (DWord(MM) * 1000 * 60) + (DWord(HH) * 1000 * 60 * 24);
end;
{$endif}



function UTF8ToSystemCharSet(const s: string): string; inline;
begin
  if SystemCharSetIsUTF8 then
    exit(s);
  {$IFDEF NoUTF8Translations}
  Result:=s;
  {$ELSE}
  Result:=UTF8ToSys(s);
  {$ENDIF}
end;


function StrToPoStr(const s:string):string;
var
  SrcPos, DestPos: Integer;
  NewLength: Integer;
begin
  NewLength:=length(s);
  for SrcPos:=1 to length(s) do
    if s[SrcPos] in ['"','\'] then inc(NewLength);
  if NewLength=length(s) then begin
    Result:=s;
  end else begin
    SetLength(Result,NewLength);
    DestPos:=1;
    for SrcPos:=1 to length(s) do begin
      case s[SrcPos] of
      '"','\':
        begin
          Result[DestPos]:='\';
          inc(DestPos);
          Result[DestPos]:=s[SrcPos];
          inc(DestPos);
        end;
      else
        Result[DestPos]:=s[SrcPos];
        inc(DestPos);
      end;
    end;
  end;
end;



{
function UpdatePOFile(Files: TStrings; const POFilename: string): boolean;
var
  InputLines: TStringList;
  Filename: string;
  BasePoFile, POFile: TSimplePOFile;
  i: Integer;
  E: EPOFileError;

  procedure UpdatePoFilesTranslation;
  var
    j: Integer;
    Lines: TStringList;
  begin
    // Update translated PO files
    Lines := FindAllTranslatedPoFiles(POFilename);
    try
      for j:=0 to Lines.Count-1 do begin
        POFile := TSimplePOFile.Create(Lines[j], true);
        try
          POFile.Tag:=1;
          POFile.UpdateTranslation(BasePOFile);
          try
            POFile.SaveToFile(Lines[j]);
          except
            on Ex: Exception do begin
              E := EPOFileError.Create(Ex.Message);
              E.ResFileName:=Lines[j];
              E.POFileName:=POFileName;
              raise E;
            end;
          end;
        finally
          POFile.Free;
        end;
      end;
    finally
      Lines.Free;
    end;
  end;

begin
  Result := false;

  if (Files=nil) or (Files.Count=0) then begin

    if FileExistsUTF8(POFilename) then begin
      // just update translated po files
      BasePOFile := TSimplePOFile.Create(POFilename, true);
      try
        UpdatePoFilesTranslation;
      finally
        BasePOFile.Free;
      end;
    end;

    exit;

  end;

  InputLines := TStringList.Create;
  try
    // Read base po items
    if FileExistsUTF8(POFilename) then
      BasePOFile := TSimplePOFile.Create(POFilename, true)
    else
      BasePOFile := TSimplePOFile.Create;
    BasePOFile.Tag:=1;

    // Update po file with lrt or/and rst files
    for i:=0 to Files.Count-1 do begin
      Filename:=Files[i];
      if (CompareFileExt(Filename,'.lrt')=0)
      or (CompareFileExt(Filename,'.rst')=0) then
        try
          //DebugLn('');
          //DebugLn(['AddFiles2Po Filename="',Filename,'"']);
          InputLines.Clear;
          InputLines.LoadFromFile(UTF8ToSys(FileName));

          if CompareFileExt(Filename,'.lrt')=0 then
            BasePOFile.UpdateStrings(InputLines, stLrt)
          else
            BasePOFile.UpdateStrings(InputLines, stRst);

        except
          on Ex: Exception do begin
            E := EPOFileError.Create(Ex.Message);
            E.ResFileName:=FileName;
            E.POFileName:=POFileName;
            raise E;
          end;
        end;
    end;
    BasePOFile.SaveToFile(POFilename);
    Result := BasePOFile.Modified;

    UpdatePOFilesTranslation;

  finally
    InputLines.Free;
    BasePOFile.Free;
  end;
end;
}

{
function Translate (Name,Value : AnsiString; Hash : Longint; arg:pointer) : AnsiString;
var
  po: TSimplePOFile;
begin
  po:=TSimplePOFile(arg);
  // get UTF8 string
  result := po.Translate(Name,Value);
  // convert UTF8 to current local
  if result<>'' then
    result:=UTF8ToSystemCharSet(result);
end;
}


{ TSimplePOFile }

{
procedure TSimplePOFile.RemoveUntaggedModules;
var
  Module: string;
  Item,VItem: TPOFileItem;
  i, p: Integer;
begin
  if FModuleList=nil then
    exit;

  // remove all module references that were not tagged
  for i:=FItems.Count-1 downto 0 do begin
    Item := TPOFileItem(FItems[i]);
    p := pos('.',Item.Identifier);
    if P=0 then
      continue; // module not found (?)

    Module :=LeftStr(Item.Identifier, p-1);
    if (FModuleList.IndexOf(Module)<0) then
      continue; // module was not modified this time

    if Item.Tag=FTag then
      continue; // PO item was updated

    // this item is not more in updated modules, delete it
    FIdentifierToItem.Remove(Item.Identifier);
    // delete it also from VarToItem
    Module := RightStr(Item.Identifier, Length(Item.Identifier)-P);
    VItem := TPoFileItem(FIdentVarToItem.Data[Module]);
    if (VItem=Item) then
      FIdentVarToItem.Remove(Module);

    FOriginalToItem.Remove(Item.Original); // isn't this tricky?
    FItems.Delete(i);
    Item.Free;
  end;
end;
}

function TSimplePOFile.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TSimplePOFile.SetCharSet(const AValue: String);
begin
  if (CompareText(FCharSet, AValue) = 0) then Exit;
  if (AValue = '') then FCharSet := 'UTF-8'
  else FCharSet := AValue;
end;



constructor TSimplePOFile.Create(const AFilename: String; const Full: Boolean = True);
var
  f: TStream;
begin
  f := TFileStream.Create(UTF8ToSys(AFilename), fmOpenRead or fmShareDenyNone);
  try
    Create(f, Full);
    if FHeader=nil then CreateHeader;
  finally
    f.Free;
  end;
end;

constructor TSimplePOFile.Create(AStream: TStream; const Full: Boolean = True);
var
  Size: Integer;
  {$ifdef ReadPoTextPChar}
  S: String;
  {$endif ReadPoTextPChar}
begin
  inherited Create;
  FAllEntries:=true;
  FItems:=TFPList.Create;
  FIdentifierToItem:=TStringHashList.Create(false);
  //FIdentVarToItem:=TStringHashList.Create(false);
  FOriginalToItem:=TStringHashList.Create(true);
  FAllEntries := Full;
  Size:=AStream.Size-AStream.Position;
  if Size<=0 then exit;
  {$ifdef ReadPoTextPChar}
  SetLength(s,Size);
  AStream.Read(S[1],Size);
  ReadPOText(S);
  {$else}
  ReadPoText(AStream);
  {$endif ReadPoTextPChar}
end;


destructor TSimplePOFile.Destroy;
var
  i: Integer;
begin
  if FModuleList<>nil then
    FModuleList.Free;
  if FHelperList<>nil then
    FHelperList.Free;
  if FHeader<>nil then
    FHeader.Free;
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Free;
  //FIdentVarToItem.Free;
  FIdentifierToItem.Free;
  FOriginalToItem.Free;
  inherited Destroy;
end;

function SliceToStr(SourceStart: PChar; SourceLen: PtrInt) : string;
//converts PChar (can be in the middle of some larger string) to a string
var
  Dest: PChar;
begin
  SetLength(Result, SourceLen);
  Dest := PChar(Result);
  System.Move(SourceStart^, Dest^, SourceLen);
end;


{$ifdef ReadPoTextPChar}
procedure TSimplePOFile.ReadPOText(const Txt: string);
{ Read a .po file. Structure:

Example
#: lazarusidestrconsts:lisdonotshowsplashscreen
msgid "Do not show splash screen"
msgstr ""

}
const
  sCommentIdentifier: PChar = '#: ';
  sCharSetIdentifier: PChar = '"Content-Type: text/plain; charset=';
  sMsgID: PChar = 'msgid "';
  sMsgStr: PChar = 'msgstr "';
  sMsgCtxt: Pchar = 'msgctxt "';
  sFlags: Pchar = '#, ';
  sPrevMsgID: PChar = '#| msgid "';
  sPrevStr: PChar = '#| "';

const
  ciNone      = 0;
  ciMsgID     = 1;
  ciMsgStr    = 2;
  ciPrevMsgID = 3;

var
  l: Integer;
  LineNr: Integer;
  LineLen: Integer;
  p: PChar;
  LineStart: PChar;
  LineEnd: PChar;
  Identifier: String;
  MsgID,MsgStr,PrevMsgID: String;
  Line: String;
  Comments: String;
  Context: string;
  Flags: string;
  TextEnd: PChar;
  i, CollectedIndex: Integer;
  OldLineStartPos: PtrUInt;
  NewSrc: String;
  s: String;

  procedure ResetVars;
  begin
    MsgId := '';
    MsgStr := '';
    Line := '';
    Identifier := '';
    Comments := '';
    Context := '';
    Flags := '';
    PrevMsgID := '';
    CollectedIndex := ciNone;
  end;

  procedure StoreCollectedLine;
  begin
    case CollectedIndex of
      ciMsgID: MsgID := Line;
      ciMsgStr: MsgStr := Line;
      ciPrevMsgID: PrevMsgID := Line;
    end;
    CollectedIndex := ciNone;
  end;

  procedure AddEntry;
  var
    Item: TPOFileItem;
  begin


    StoreCollectedLine;
    if Identifier<>'' then begin
      // check for unresolved duplicates in po file
      {
      Item := TPOFileItem(FOriginalToItem.Data[MsgID]);
      if (Item<>nil) then begin
        // fix old duplicate context
        if Item.Context='' then
          Item.Context:=Item.Identifier;
        // set context of new duplicate
        if Context='' then
          Context := Identifier;
        // if old duplicate was translated and
        // new one is not, provide a initial translation
        if MsgStr='' then
          MsgStr := Item.Translation;
      end;
      }
      Add(Identifier,MsgID,MsgStr,Comments,Context,Flags,PrevMsgID, LineNr);
      ResetVars;
    end else
    if (Line<>'') and (FHeader=nil) then begin
      FHeader := TPOFileItem.Create('',MsgID,Line);
      FHeader.Comments:=Comments;
      ResetVars;
    end
  end;

  function TestPrefixStr(AIndex: Integer): boolean;
  var
    s: string;
    l: Integer;
  begin
    case aIndex of
      ciMsgID: s:=sMsgId;
      ciMsgStr: s:=sMsgStr;
      ciPrevMsgId: s:=sPrevMsgId;
    end;
    L := Length(s);
    result := CompareMem(LineStart, pchar(s), L);
    if Result then begin
      StoreCollectedLine;
      CollectedIndex := AIndex;
      Line:=SliceToStr(LineStart+L,LineLen-L-1);
    end;
  end;

begin
  {$ifdef DebugSimplePoFiles}
  T0 := GetTickCount;
  {$endif}
  if Txt='' then exit;
  s:=Txt;
  l:=length(s);
  p:=PChar(s);
  LineStart:=p;
  TextEnd:=p+l;

  Identifier:='';
  Comments:='';
  Line:='';
  Flags:='';
  CollectedIndex := ciNone;
  LineNr := 0;
  while LineStart<TextEnd do begin
    Inc(LineNr);


    LineEnd:=LineStart;
    while (not (LineEnd^ in [#0,#10,#13])) do inc(LineEnd);
    LineLen:=LineEnd-LineStart;
    if LineLen>0 then begin

      if CompareMem(LineStart,sCommentIdentifier,3) then begin
        AddEntry;
        Identifier:=copy(s,LineStart-p+4,LineLen-3);
        // the RTL creates identifier paths with point instead of colons
        // fix it:
        for i:=1 to length(Identifier) do
          if Identifier[i]=':' then
            Identifier[i]:='.';
      end else if TestPrefixStr(ciMsgId) then begin
      end else if TestPrefixStr(ciMsgStr) then begin
      end else if TestPrefixStr(ciPrevMsgId) then begin
      end else if CompareMem(LineStart, sMsgCtxt,9) then begin
        Context:= Copy(LineStart, 10, LineLen-10);
      end else if CompareMem(LineStart, sFlags, 3) then begin
        Flags := copy(LineStart, 4, LineLen-3);
      end else if (LineStart^='"') then begin
        if (MsgID='') and CompareMem(LineStart,sCharSetIdentifier,35) then
        begin

          SetCharSet(copy(LineStart,36,LineLen-38));
          {if SysUtils.CompareText(FCharSet,'UTF-8')<>0 then begin
            // convert encoding to UTF-8
            OldLineStartPos:=PtrUInt(LineStart-PChar(s))+1;
            NewSrc:=ConvertEncoding(copy(s,OldLineStartPos,length(s)),
                                    FCharSet,EncodingUTF8);
            // replace text and update all pointers
            s:=copy(s,1,OldLineStartPos-1)+NewSrc;
            l:=length(s);
            p:=PChar(s);
            TextEnd:=p+l;
            LineStart:=p+(OldLineStartPos-1);
            LineEnd:=LineStart;
            while (not (LineEnd^ in [#0,#10,#13])) do inc(LineEnd);
            LineLen:=LineEnd-LineStart;
          end;
          }
        end;
        Line := Line + SliceToStr(LineStart+1,LineLen-2);
      end else if CompareMem(LineStart, sPrevStr, 4) then begin
        Line := Line + SliceToStr(LineStart+5,LineLen-6);
      end else if LineStart^='#' then begin
        if Comments<>'' then
          Comments := Comments + LineEnding;
        Comments := Comments + Copy(LineStart, 1, LineLen);
      end else
        AddEntry;
    end
    else Inc(LineNr);
    LineStart:=LineEnd+1;
    while (LineStart<TextEnd) and (LineStart^ in [#10,#13]) do inc(LineStart);
  end;
  AddEntry;

  {$ifdef DebugSimplePoFiles}
  T1 := gettickcount;
  debugln('T1 = ',dbgs(t1-t0));
  debugln('Count = ',DbgS(Count));
  //debugln('T2 = ',dbgs(t2-t1));
  //debugln('T3 = ',dbgs(t3-t2));
  {$endif}

end;

{$else}

procedure TSimplePOFile.ReadPOText(AStream: TStream);
{ Read a .po file. Structure:

Example
#: lazarusidestrconsts:lisdonotshowsplashscreen
msgid "Do not show splash screen"
msgstr ""

}
const
  sCommentIdentifier = '#: ';
  lCommentIdentifier = 3;
  sCharSetIdentifier = '"Content-Type: text/plain; charset=';
  lCharSetIdentifier = 35;
  sMsgID = 'msgid "';
  //lMsgId = 7;
  sMsgStr = 'msgstr "';
  //lMsgStr = 8;
  sMsgCtxt = 'msgctxt "';
  lMsgCtxt = 9;
  sFlags = '#, ';
  lFlags = 3;
  sPrevMsgID = '#| msgid "';
  //lPrevMsgId = 11;
  sPrevStr = '#| "';
  lPrevStr = 4;

const
  ciNone      = 0;
  ciMsgID     = 1;
  ciMsgStr    = 2;
  ciPrevMsgID = 3;

var
  SL: TStringList;
  //l: Integer;
  Cnt: Integer;
  LineLen: Integer;
  LineNr: Integer;
  p: Integer;
  //LineStart: PChar;
  //LineEnd: PChar;
  Identifier: String;
  MsgID,MsgStr,PrevMsgID: String;
  Line: String;
  Comments: String;
  Context: string;
  Flags: string;
  //TextEnd: PChar;
  i, CollectedIndex: Integer;
  //OldLineStartPos: PtrUInt;
  //NewSrc: String;
  //s: String;
  CurLine: String;

  procedure ResetVars;
  begin
    MsgId := '';
    MsgStr := '';
    Line := '';
    Identifier := '';
    Comments := '';
    Context := '';
    Flags := '';
    PrevMsgID := '';
    CollectedIndex := ciNone;
    LineNr := 0;
  end;

  procedure StoreCollectedLine;
  begin
    case CollectedIndex of
      ciMsgID: MsgID := Line;
      ciMsgStr: MsgStr := Line;
      ciPrevMsgID: PrevMsgID := Line;
    end;
    CollectedIndex := ciNone;
  end;

  procedure AddEntry(const LineNr: Integer);
  //var
    //Item: TPOFileItem;
  begin


    StoreCollectedLine;
    if Identifier<>'' then
    begin
      // check for unresolved duplicates in po file
      {
      Item := TPOFileItem(FOriginalToItem.Data[MsgID]);
      if (Item<>nil) then begin
        // fix old duplicate context
        if Item.Context='' then
          Item.Context:=Item.Identifier;
        // set context of new duplicate
        if Context='' then
          Context := Identifier;
        // if old duplicate was translated and
        // new one is not, provide a initial translation
        if MsgStr='' then
          MsgStr := Item.Translation;
      end;
      }
      Add(Identifier,MsgID,MsgStr,Comments,Context,Flags,PrevMsgID, LineNr);
      ResetVars;
    end else
    if (Line<>'') and (FHeader=nil) then
    begin
      FHeader := TPOFileItem.Create('',MsgID,Line);
      FHeader.Comments:=Comments;
      ResetVars;
    end
  end;

  function TestPrefixStr(AIndex: Integer): boolean;
  var
    s: string;
    l: Integer;
  begin
    case aIndex of
      ciMsgID: s:=sMsgId;
      ciMsgStr: s:=sMsgStr;
      ciPrevMsgId: s:=sPrevMsgId;
    end;
    L := Length(s);
    result := Pos(S, CurLine) = 1;
    if Result then
    begin
      StoreCollectedLine;
      CollectedIndex := AIndex;
      Line := Copy(CurLine,L+1,LineLen-L-1);
    end;
  end;

begin
  {$ifdef DebugSimplePoFiles}
  T0 := GetTickCount;
  {$endif}
  SL := TStringList.Create;
  SL.LoadFromStream(AStream);
  try
    if SL.Count > 0 then AdjustLinebreaks(SL.Text);
    Identifier:='';
    Comments:='';
    Line:='';
    Flags:='';
    CollectedIndex := ciNone;
    LineNr := 0;

    for Cnt := 0 to SL.Count - 1 do
    begin
      CurLine := Sl.Strings[Cnt];


      LineLen := Length(CurLine);
      if (LineLen > 0) then
      begin
        p := Pos(sCommentIdentifier,CurLine);
        if  (p = 1) then
        begin
          //Add the Entry collected before this line (not the current line)
          AddEntry(LineNr);
          LineNr := Cnt + 1;
          Identifier:=copy(CurLine,lCommentIdentifier+1,LineLen-lCommentIdentifier);
          // the RTL creates identifier paths with point instead of colons
          // fix it:
          for i:=1 to length(Identifier) do
            if Identifier[i]=':' then
              Identifier[i]:='.';
        end
        else if TestPrefixStr(ciMsgId) then
        begin
        end
        else if TestPrefixStr(ciMsgStr) then
        begin
        end
        else if TestPrefixStr(ciPrevMsgId) then
        begin
        end else if (Pos(sMsgCtxt, CurLine) = 1) then
        begin
          Context:= Copy(CurLine,lMsgCtxt+1,LineLen - lMsgCtxt - 1);
        end
        else if Pos(SFlags, CurLine) = 1 then
        begin
          Flags := Copy(CurLine, lFlags + 1, LineLen - lFlags);
        end
        else if (CurLine[1] = '"') then
        begin
          if (MsgID='') and (Pos(sCharSetIdentifier,CurLine) = 1) then
          begin

            SetCharSet(copy(CurLine,lCharSetIdentifier+1,LineLen-lCharSetIdentifier-3));
            {if SysUtils.CompareText(FCharSet,'UTF-8')<>0 then begin
              // convert encoding to UTF-8
              OldLineStartPos:=PtrUInt(LineStart-PChar(s))+1;
              NewSrc:=ConvertEncoding(copy(s,OldLineStartPos,length(s)),
                                      FCharSet,EncodingUTF8);
              // replace text and update all pointers
              s:=copy(s,1,OldLineStartPos-1)+NewSrc;
              l:=length(s);
              p:=PChar(s);
              TextEnd:=p+l;
              LineStart:=p+(OldLineStartPos-1);
              LineEnd:=LineStart;
              while (not (LineEnd^ in [#0,#10,#13])) do inc(LineEnd);
              LineLen:=LineEnd-LineStart;
            end;
            }
          end;
          Line := Line + Copy(CurLine,2,LineLen-2);
        end
        else if Pos(sPrevStr,CurLine) = 1 then
        begin
          Line := Line + Copy(CurLine,lPrevStr + 1,LineLen - lPrevStr - 1);
        end
        else if CurLine[1] = '#' then
        begin
          if Comments<>'' then Comments := Comments + LineEnding;
          Comments := Comments + CurLine;
        end
        else
        begin
          AddEntry(LineNr);
        end;
      end;//LineLen > 0
    end;
    //debugln('Last entry:');
    //debugln('Identifier = ',Identifier);
    //debugln('LineNr = ',DbgS(LineNr));
    //debugln('Cnt = ',DbgS(Cnt));

    AddEntry(LineNr);
  finally
    SL.Free;
  end;

  {$ifdef DebugSimplePoFiles}
  T1 := gettickcount;
  debugln('T1 = ',dbgs(t1-t0));
  debugln('Count = ',DbgS(Count));
  {$endif}
end;

{$endif ReadPoTextPChar}

procedure TSimplePOFile.Add(const Identifier, OriginalValue, TranslatedValue,
  Comments, Context, Flags, PreviousID: string; LineNr: Integer);
var
  Item, OItem: TPOFileItem;
  OIndex: Integer;
  //p: Integer;
begin
  if (not FAllEntries) and (TranslatedValue='') then exit;

  Item:=TPOFileItem.Create(Identifier,OriginalValue,TranslatedValue);
  Item.Comments:=Comments;
  Item.Context:=Context;
  Item.Flags:=Flags;
  Item.PreviousID:=PreviousID;
  Item.Tag:=0;
  Item.LineNr := LineNr;
  FItems.Add(Item);

  //debugln('TPOFile.Add %8x Tag=%d Id="%s" Org="%s" Trn="%s"',
  //    [ptrint(Item),FTag,Identifier,dbgstr(OriginalValue),dbgstr(TranslatedValue)]);


  FIdentifierToItem.Add(Identifier,Item);


  {
  P := Pos('.', Identifier);
  if P>0 then
    FIdentVarToItem.Add(copy(Identifier, P+1, Length(IDentifier)), Item);
  }

  //if FIdentifierToItem.Data[UpperCase(Identifier)]=nil then raise Exception.Create('');
  OIndex := FOriginalToItem.Find(OriginalValue);
  if (OIndex > -1) then
  begin
    //TPoFileItem(FOriginalToItem.List[OIndex]^.Data).Tag := TPoFileItem(FOriginalToItem.List[OIndex]^.Data).Tag or tgHasDup;
    OItem := TPoFileItem(FOriginalToItem.List[OIndex]^.Data);
    OItem.Tag := OItem.Tag or tgHasDup;
    Item.Tag := Item.Tag or tgHasDup;
  end;
  FOriginalToItem.Add(OriginalValue,Item);
  //if FOriginalToItem.Data[OriginalValue]=nil then raise Exception.Create('');
end;

{
function TSimplePOFile.Translate(const Identifier, OriginalValue: String): String;
var
  Item: TPOFileItem;
begin
  Item:=TPOFileItem(FIdentifierToItem.Data[Identifier]);
  if Item=nil then
    Item:=TPOFileItem(FOriginalToItem.Data[OriginalValue]);
  if Item<>nil then begin
    Result:=Item.Translation;
    if Result='' then RaiseGDBException('TPOFile.Translate Inconsistency');
  end else
    Result:=OriginalValue;
end;
}

procedure TSimplePOFile.Report;
begin
  Report(0, Count - 1, True);
end;

procedure TSimplePOFile.Report(StartIndex, StopIndex: Integer;
  const DisplayHeader: Boolean);
var
  Item: TPOFileItem;
  i: Integer;
begin
  if DisplayHeader then
  begin
    DebugLn('Header:');
    DebugLn('---------------------------------------------');

    if FHeader=nil then
      DebugLn('No header found in po file')
    else begin
      DebugLn('Comments=',FHeader.Comments);
      DebugLn('Identifier=',FHeader.Identifier);
      DebugLn('msgid=',FHeader.Original);
      DebugLn('msgstr=', FHeader.Translation);
    end;
    DebugLn;
  end;

  if (StartIndex > StopIndex) then
  begin
    i := StopIndex;
    StopIndex := StartIndex;
    StartIndex := i;
  end;
  if (StopIndex > Count - 1) then StopIndex := Count - 1;
  if (StartIndex < 0) then StartIndex := 0;

  DebugLn('Entries [',DbgS(StartIndex),'..',Dbgs(StopIndex),']:');
  DebugLn('---------------------------------------------');
  for i := StartIndex to StopIndex do begin
    DebugLn('#',dbgs(i),': ');
    Item := TPOFileItem(FItems[i]);
    DebugLn('Identifier=',Item.Identifier);
    DebugLn('msgid=',Item.Original);
    DebugLn('msgstr=', Item.Translation);
    DebugLn('Comments=',Item.Comments);
    DebugLn;
  end;
end;

procedure TSimplePOFile.Report(Log: TStrings; StartIndex, StopIndex: Integer;
  const DisplayHeader: Boolean);
var
  Item: TPOFileItem;
  i: Integer;
begin
  if DisplayHeader then
  begin
    Log.Add('Header:');
    Log.Add('---------------------------------------------');

    if FHeader=nil then
      Log.Add('No header found in po file')
    else begin
      Log.Add('Comments='+FHeader.Comments);
      Log.Add('Identifier='+FHeader.Identifier);
      Log.Add('msgid='+FHeader.Original);
      Log.Add('msgstr='+ FHeader.Translation);
    end;
    Log.Add('');
  end;

  if (StartIndex > StopIndex) then
  begin
    i := StopIndex;
    StopIndex := StartIndex;
    StartIndex := i;
  end;
  if (StopIndex > Count - 1) then StopIndex := Count - 1;
  if (StartIndex < 0) then StartIndex := 0;

  Log.Add('Entries ['+DbgS(StartIndex)+'..'+Dbgs(StopIndex)+']:');
  Log.Add('---------------------------------------------');
  for i := StartIndex to StopIndex do begin
    Log.Add('#'+dbgs(i)+': ');
    Item := TPOFileItem(FItems[i]);
    Log.Add('Identifier='+Item.Identifier);
    Log.Add('msgid='+Item.Original);
    Log.Add('msgstr='+ Item.Translation);
    Log.Add('Comments='+Item.Comments);
    Log.Add('');
  end;
end;

procedure TSimplePOFile.CreateHeader;
begin
  if FHeader=nil then
    FHeader := TPOFileItem.Create('','','');
  FHeader.Translation:='Content-Type: text/plain; charset=UTF-8';
  FHeader.Comments:='';
end;

{
procedure TSimplePOFile.UpdateStrings(InputLines: TStrings; SType: TStringsType);
var
  i,j,n: integer;
  p: LongInt;
  Identifier, Value,Line: string;
  Ch: Char;
  MultiLinedValue: boolean;

  procedure NextLine;
  begin
    if i<InputLines.Count then
      inc(i);
    if i<InputLines.Count then
      Line := InputLines[i]
    else
      Line := '';
    n := Length(Line);
    p := 1;
  end;

begin
  ClearModuleList;
  UntagAll;
  // for each string in lrt/rst list check if it's already
  // in PO if not add it
  Value := '';
  Identifier := '';
  i := 0;
  while i < InputLines.Count do begin

    Line := InputLines[i];
    n := Length(Line);

    if n=0 then
      // empty line
    else
    if SType=stLrt then begin

      p:=Pos('=',Line);
      Value :=copy(Line,p+1,n-p); //if p=0, that's OK, all the string
      Identifier:=copy(Line,1,p-1);
      UpdateItem(Identifier, Value);

    end else begin
      // rst file
      if Line[1]='#' then begin
        // rst file: comment

        Value := '';
        Identifier := '';
        MultilinedValue := false;

      end else begin

        p:=Pos('=',Line);
        if P>0 then begin

          Identifier := copy(Line,1,p-1);
          inc(p); // points to ' after =

          Value := '';
          while p<=n do begin

            if Line[p]='''' then begin
              inc(p);
              j:=p;
              while (p<=n)and(Line[p]<>'''') do
                inc(p);
              Value := Value + copy(Line, j, P-j);
              inc(p);
              continue;
            end else
            if Line[p] = '#' then begin
              // a #decimal
              repeat
                inc(p);
                j:=p;
                while (p<=n)and(Line[p] in ['0'..'9']) do
                  inc(p);

                Ch := Chr(StrToInt(copy(Line, j, p-j)));
                Value := Value + Ch;
                if Ch in [#13,#10] then
                  MultilinedValue := True;

                if (p=n) and (Line[p]='+') then
                  NextLine;

              until (p>n) or (Line[p]<>'#');
            end else
            if Line[p]='+' then
              NextLine
            else
              inc(p); // this is an unexpected string
          end;

          if Value<>'' then begin
            if MultiLinedValue then begin
              // check that we end on lineending, multilined
              // resource strings from rst usually do not end
              // in lineending, fix here.
              if not (Value[Length(Value)] in [#13,#10]) then
                Value := Value + LineEnding;
            end;
            // po requires special characters as #number
            p:=1;
            while p<=length(Value) do begin
              j := UTF8CharacterLength(pchar(@Value[p]));
              if (j=1) and (Value[p] in [#0..#9,#11,#12,#14..#31,#127..#255]) then
                Value := copy(Value,1,p-1)+'#'+IntToStr(ord(Value[p]))+copy(Value,p+1,length(Value))
              else
                inc(p,j);
            end;

            UpdateItem(Identifier, Value);
          end;

        end; // if p>0 then begin
      end;
    end;

    inc(i);
  end;

  RemoveUntaggedModules;
end;
}


{
procedure TSimplePOFile.RemoveTaggedItems(aTag: Integer);
var
  Item: TPOFileItem;
  i: Integer;
begin
  // get rid of all entries that have Tag=aTag
  for i:=FItems.Count-1 downto 0 do begin
    Item := TPOFileItem(FItems[i]);
    if Item.Tag<>aTag then
      Continue;
    FIdentifierToItem.Remove(Item.Identifier);
    FOriginalToItem.Remove(Item.Original); // isn't this tricky?
    FItems.Delete(i);
    Item.Free;
  end;
end;
}

function ComparePOItems(Item1, Item2: Pointer): Integer;
begin
  result := CompareText(TPOFileItem(Item1).Identifier,
                        TPOFileItem(Item2).Identifier);
end;

{
procedure TSimplePOFile.SaveToFile(const AFilename: string);
var
  OutLst: TStringList;
  j: Integer;

  procedure WriteLst(const AProp, AValue: string );
  var
    i: Integer;
    s: string;
  begin
    if (AValue='') and (AProp='') then
      exit;

    FHelperList.Text:=AValue;
    if FHelperList.Count=1 then begin
      if AProp='' then OutLst.Add(FHelperList[0])
      else             OutLst.Add(AProp+' "'+FHelperList[0]+'"');
    end else begin
      if AProp<>'' then
        OutLst.Add(AProp+' ""');
      for i:=0 to FHelperList.Count-1 do begin
        s := FHelperList[i];
        if AProp<>'' then begin
          s := '"' + s + '\n"';
          if AProp='#| msgid' then
            s := '#| ' + s;
        end;
        OutLst.Add(s)
      end;
    end;
  end;

  procedure WriteItem(Item: TPOFileItem);
  begin
    WriteLst('',Item.Comments);
    if Item.Identifier<>'' then
      OutLst.Add('#: '+Item.Identifier);
    if Trim(Item.Flags)<>'' then
      OutLst.Add('#, '+Trim(Item.Flags));
    if Item.PreviousID<>'' then
      WriteLst('#| msgid', strToPoStr(Item.PreviousID));
    if Item.Context<>'' then
      WriteLst('msgctxt', Item.Context);
    WriteLst('msgid', StrToPoStr(Item.Original));
    WriteLst('msgstr', StrToPoStr(Item.Translation));
    OutLst.Add('');
  end;

begin
  if FHeader=nil then
    CreateHeader;

  if FHelperList=nil then
    FHelperList:=TStringList.Create;

  OutLst := TStringList.Create;
  try
    // write header
    WriteItem(FHeader);

    // Sort list of items by identifier
    FItems.Sort(@ComparePOItems);

    for j:=0 to Fitems.Count-1 do
      WriteItem(TPOFileItem(FItems[j]));

    OutLst.SaveToFile(UTF8ToSys(AFilename));

  finally
    OutLst.Free;
  end;

end;
}

function SkipLineEndings(var P: PChar; var DecCount: Integer): Integer;
  procedure Skip;
  begin
    Dec(DecCount);
    Inc(P);
  end;
begin
  Result  := 0;
  while (P^ in [#10,#13]) do begin
    Inc(Result);
    if (P^=#13) then begin
      Skip;
      if P^=#10 then
        Skip;
    end else
      Skip;
  end;
end;

function CompareMultilinedStrings(const S1,S2: string): Integer;
var
  C1,C2,L1,L2: Integer;
  P1,P2: PChar;
begin
  L1 := Length(S1);
  L2 := Length(S2);
  P1 := pchar(S1);
  P2 := pchar(S2);
  Result := ord(P1^) - ord(P2^);

  while (Result=0) and (P1^<>#0) do begin
    Inc(P1); Inc(P2);
    Dec(L1); Dec(L2);
    if P1^<>P2^ then begin
      C1 := SkipLineEndings(P1, L1);
      C2 := SkipLineEndings(P2, L2);
      if (C1<>C2) then
        // different amount of lineendings
        result := C1-C2
      else
      if (C1=0) then
        // there are no lineendings at all, will end loop
        result := Ord(P1^)-Ord(P2^);
    end;
  end;

  // if strings are the same, check that all chars have been consumed
  // just in case there are unexpected chars in between, in this case
  // L1=L2=0;
  if Result=0 then
    Result := L1-L2;
end;

{
procedure TSimplePOFile.UpdateItem(const Identifier: string; Original: string);
var
  Item: TPOFileItem;
  AContext,AComment,ATranslation,AFlags,APrevStr: string;
begin
  if FHelperList=nil then
    FHelperList := TStringList.Create;

  // try to find PO entry by identifier
  Item:=TPOFileItem(FIdentifierToItem.Data[Identifier]);
  if Item<>nil then begin
    // found, update item value
    AddToModuleList(IDentifier);

    if CompareMultilinedStrings(Item.Original, Original)<>0 then begin
      FModified := True;
      if Item.Translation<>'' then begin
        Item.ModifyFlag('fuzzy', true);
        Item.PreviousID:=Item.Original;
      end;
    end;
    Item.Original:=Original;
    Item.Tag:=FTag;
    exit;
  end;

  // try to find po entry based only on it's value
  AContext := '';
  AComment := '';
  ATranslation := '';
  AFlags := '';
  APrevStr := '';
  Item := TPOFileItem(FOriginalToItem.Data[Original]);
  if Item<>nil then begin
    // old item don't have context, add one
    if Item.Context='' then
      Item.Context := Item.Identifier;

    // if old item it's already translated use translation
    if Item.Translation<>'' then
      ATranslation := Item.Translation;

    AFlags := Item.Flags;
    // if old item was fuzzy, new should be fuzzy too.
    if (ATranslation<>'') and (pos('fuzzy', AFlags)<>0) then
      APrevStr := Item.PreviousID;

    // update identifier list
    AContext := Identifier;
  end;

  // this appear to be a new item
  FModified := true;
  Add(Identifier, Original, ATranslation, AComment, AContext, AFlags, APrevStr);
end;
}

{
procedure TSimplePOFile.UpdateTranslation(BasePOFile: TSimplePOFile);
var
  Item: TPOFileItem;
  i: Integer;
begin
  UntagAll;
  ClearModuleList;
  for i:=0 to BasePOFile.Items.Count-1 do begin
    Item := TPOFileItem(BasePOFile.Items[i]);
    UpdateItem(Item.Identifier, Item.Original);
  end;
  RemoveTaggedItems(0); // get rid of any item not existing in BasePOFile
end;
}

{
procedure TSimplePOFile.ClearModuleList;
begin
  if FModuleList<>nil then
    FModuleList.Clear;
end;
}

{
procedure TSimplePOFile.AddToModuleList(Identifier: string);
var
  p: Integer;
begin
  if FModuleList=nil then begin
    FModuleList := TStringList.Create;
    FModuleList.Duplicates:=dupIgnore;
  end;
  p := pos('.', Identifier);
  if p>0 then
    FModuleList.Add(LeftStr(Identifier, P-1));
end;
}

{
procedure TSimplePOFile.UntagAll;
var
  Item: TPOFileItem;
  i: Integer;
begin
  for i:=0 to Items.Count-1 do begin
    Item := TPOFileItem(Items[i]);
    Item.Tag:=0;
  end;
end;
}

function TSimplePOFile.FindPoItem(const Identifier: String): TPoFileItem;
begin
  Result := TPOFileItem(FIdentifierToItem.Data[Identifier]);
end;

function TSimplePOFile.GetPoItem(Index: Integer): TPoFileItem;
begin
  Result := TPoFileItem(FItems.Items[Index]);
end;

function TSimplePOFile.OriginalToItem(Data: String): TPoFileItem;
begin
  Result := TPOFileItem(FOriginalToItem.Data[Data]);
end;

{ TPOFileItem }

constructor TPOFileItem.Create(const TheIdentifier, TheOriginal,
  TheTranslated: string);
begin
  Identifier:=TheIdentifier;
  Original:=TheOriginal;
  Translation:=TheTranslated;
end;

procedure TPOFileItem.ModifyFlag(const AFlag: string; Check: boolean);
var
  i: Integer;
  F: TStringList;
begin
  F := TStringList.Create;
  try

    F.CommaText := Flags;
    i := F.IndexOf(AFlag);

    if (i<0) and Check then
      F.Add(AFlag)
    else
    if (i>=0) and (not Check) then
      F.Delete(i);

    Flags := F.CommaText;

  finally
    F.Free;
  end;
end;

end.


