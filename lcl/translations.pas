{ $Id$}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Methods and classes for loading translations/localizations from po files.

  Example 1: Load a specific .po file:

    procedure TForm1.FormCreate(Sender: TObject);
    var
      PODirectory: String;
    begin
      PODirectory:='/path/to/lazarus/lcl/languages/';
      TranslateUnitResourceStrings('LCLStrConsts',PODirectory+'lcl.%s.po',
                                   'nl','');
      MessageDlg('Title','Text',mtInformation,[mbOk,mbCancel,mbYes],0);
    end;


  Example 2: Load the current language file using the GetLanguageIDs function
    of the gettext unit:
  
    procedure TForm1.FormCreate(Sender: TObject);
    var
      PODirectory, Lang, FallbackLang: String;
    begin
      PODirectory:='/path/to/lazarus/lcl/languages/';
      GetLanguageIDs(Lang,FallbackLang); // in unit gettext
      TranslateUnitResourceStrings('LCLStrConsts',PODirectory+'lcl.%s.po',
                                   Lang,FallbackLang);
      MessageDlg('Title','Text',mtInformation,[mbOk,mbCancel,mbYes],0);
    end;
}
unit Translations;

{$mode objfpc}{$H+}{$INLINE ON}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, StringHashList
  {$IFDEF UNIX}{$IFNDEF DisableCWString}, cwstring{$ENDIF}{$ENDIF};

type
  TStringsType = (stLrt, stRst);

type
  { TPOFileItem }

  TPOFileItem = class
  public
    Tag: Integer;
    Comments: string;
    Identifier: string;
    Original: string;
    Translation: string;
    Context: string;
    constructor Create(const TheIdentifier, TheOriginal, TheTranslated: string);
  end;

  { TPOFile }

  TPOFile = class
  protected
    FItems: TFPList;// list of TPOFileItem
    FIdentifierToItem: TStringHashList;
    FIdentVarToItem: TStringHashList;
    FOriginalToItem: TStringHashList;
    FCharSet: String;
    FHeader: TPOFileItem;
    FAllEntries: boolean;
    FTag: Integer;
    FModified: boolean;
    FHelperList: TStringList;
    FModuleList: TStringList;
    procedure RemoveTaggedItems(aTag: Integer);
    procedure RemoveUntaggedModules;
  public
    constructor Create;
    constructor Create(const AFilename: String; Full:boolean=false);
    constructor Create(AStream: TStream; Full:boolean=false);
    destructor Destroy; override;
    procedure ReadPOText(const s: string);
    procedure Add(const Identifier, OriginalValue, TranslatedValue, Comments,
                        Context: string);
    function Translate(const Identifier, OriginalValue: String): String;
    Property CharSet: String read FCharSet;
    procedure Report;
    procedure CreateHeader;
    procedure UpdateStrings(InputLines:TStrings; SType: TStringsType);
    procedure SaveToFile(const AFilename: string);
    procedure UpdateItem(const Identifier: string; Original: string);
    procedure UpdateTranslation(BasePOFile: TPOFile);
    procedure ClearModuleList;
    procedure AddToModuleList(Identifier: string);
    procedure UntagAll;
    
    property Tag: integer read FTag write FTag;
    property Modified: boolean read FModified;
    property Items: TFPList read FItems;
    
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
procedure TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string);
function TranslateUnitResourceStrings(const ResUnitName, AFilename: string
  ): boolean;
function UTF8ToSystemCharSet(const s: string): string; inline;

function UpdatePoFile(Files: TStrings; const POFilename: string): boolean;


implementation

function UTF8ToSystemCharSet(const s: string): string; inline;
begin
  if SystemCharSetIsUTF8 then
    exit(s);
  {$IFDEF NoUTF8Translations}
  Result:=s;
  {$ELSE}
  Result:=Utf8ToAnsi(s);
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

function FindAllTranslatedPoFiles(const Filename: string): TStringList;
var
  Path: String;
  Name: String;
  NameOnly: String;
  Ext: String;
  FileInfo: TSearchRec;
  CurExt: String;
begin
  Result:=TStringList.Create;
  Path:=ExtractFilePath(Filename);
  Name:=ExtractFilename(Filename);
  Ext:=ExtractFileExt(Filename);
  NameOnly:=LeftStr(Name,length(Name)-length(Ext));
  if SysUtils.FindFirst(Path+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      or (CompareFilenames(FileInfo.Name,Name)=0) then continue;
      CurExt:=ExtractFileExt(FileInfo.Name);
      if (CompareFilenames(CurExt,'.po')<>0)
      or (CompareFilenames(LeftStr(FileInfo.Name,length(NameOnly)),NameOnly)<>0)
      then
        continue;
      Result.Add(Path+FileInfo.Name);
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
end;

function UpdatePOFile(Files: TStrings; const POFilename: string): boolean;
var
  InputLines: TStringList;
  Filename: string;
  BasePoFile, POFile: TPoFile;
  i: Integer;
  E: EPOFileError;
begin
  Result := false;
  
  if (Files=nil) or (Files.Count=0) then
    exit;

  InputLines := TStringList.Create;
  try
    // Read base po items
    if FileExists(POFilename) then
      BasePOFile := TPOFile.Create(POFilename, true)
    else
      BasePOFile := TPOFile.Create;
    BasePOFile.Tag:=1;

    // Update po file with lrt or/and rst files
    for i:=0 to Files.Count-1 do begin
      Filename:=Files[i];
      if (CompareFileExt(Filename,'.lrt')=0)
      or (CompareFileExt(Filename,'.rst')=0) then
        //DebugLn(['AddFiles2Po Filename="',Filename,'"']);
        
        try
          InputLines.Clear;
          InputLines.LoadFromFile(FileName);

          if CompareFileExt(Filename,'.lrt')=0 then
            BasePOFile.UpdateStrings(InputLines, stLrt)
          else
            BasePOFile.UpdateStrings(InputLines, stRst);

        except
          E := EPOFileError.Create('');
          E.ResFileName:=FileName;
          E.POFileName:=POFileName;
          raise E;
        end;
        
    end;
    BasePOFile.SaveToFile(POFilename);
    Result := BasePOFile.Modified;

    // Update translated PO files
    InputLines.Free;
    InputLines := FindAllTranslatedPoFiles(POFilename);
    for i:=0 to InputLines.Count-1 do begin
      POFile := TPOFile.Create(InputLines[i], true);
      try
        POFile.Tag:=1;
        POFile.UpdateTranslation(BasePOFile);
        try
          POFile.SaveToFile(InputLines[i]);
        except
          E := EPOFileError.Create('');
          E.ResFileName:=InputLines[i];
          E.POFileName:=POFileName;
          raise E;
        end;
      finally
        POFile.Free;
      end;
    end;
    
  finally
    InputLines.Free;
    BasePOFile.Free;
  end;
end;

{$ifndef ver2_0}
function Translate (Name,Value : AnsiString; Hash : Longint; arg:pointer) : AnsiString;
var
  po: TPOFile;
begin
  po:=TPOFile(arg);
  // get UTF8 string
  result := po.Translate(Name,Value);
  // convert UTF8 to current local
  if result<>'' then
    result:=UTF8ToSystemCharSet(result);
end;
{$endif ver2_0}

function TranslateUnitResourceStrings(const ResUnitName, AFilename: string
  ): boolean;
var
{$ifdef ver2_0}
  TableID, StringID, TableCount: Integer;
  s: String;
  DefValue: String;
{$endif ver2_0}
  po: TPOFile;
begin
  Result:=false;
  //debugln('TranslateUnitResourceStrings) ResUnitName="',ResUnitName,'" AFilename="',AFilename,'"');
  if (ResUnitName='') or (AFilename='') or (not FileExists(AFilename)) then
    exit;
  try
    po := nil;
    // read .po file
    po := TPOFile.Create(AFilename);
    try
{$ifdef ver2_0}
      for TableID:=0 to ResourceStringTableCount - 1 do begin
        TableCount := ResourceStringCount(TableID);

        // check if this table belongs to the ResUnitName
        if TableCount=0 then continue;
        s:=GetResourceStringName(TableID,0);
        if CompareText(ResUnitName+'.',LeftStr(s,length(ResUnitName)+1))<>0
        then continue;
        
        // translate all resource strings of the unit
        for StringID := 0 to TableCount - 1 do begin
          DefValue:=GetResourceStringDefaultValue(TableID,StringID);
          // get UTF8 string
          s := po.Translate(GetResourceStringName(TableID,StringID),DefValue);

          if Length(s) > 0 then begin
            // convert UTF8 to current local
            s:=UTF8ToSystemCharSet(s);
            SetResourceStringValue(TableID,StringID,s);
          end;
        end;
      end;
{$else ver2_0}
      SetUnitResourceStrings(ResUnitName,@Translate,po);
{$endif ver2_0}
    finally
      po.Free;
    end;
    Result:=true;
  except
    on e: Exception do begin
      DebugLn('Exception while translating ', ResUnitName);
      DebugLn(e.Message);
      DumpExceptionBackTrace;
    end;
  end;
end;

procedure TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string);
begin
  if (ResUnitName='') or (BaseFilename='') then exit;

  //debugln('TranslateUnitResourceStrings BaseFilename="',BaseFilename,'"');
  if (FallbackLang<>'') then
    TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[FallbackLang]));
  if (Lang<>'') then
    TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[Lang]));
end;

{ TPOFile }

procedure TPOFile.RemoveUntaggedModules;
var
  Module: string;
  Item: TPOFileItem;
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
    //FOriginalToItem.Remove(Item.Original); // isn't this tricky?
    FItems.Delete(i);
    Item.Free;
  end;
end;

constructor TPOFile.Create;
begin
  inherited Create;
  FAllEntries:=true;
  FItems:=TFPList.Create;
  FIdentifierToItem:=TStringHashList.Create(false);
  FIdentVarToItem:=TStringHashList.Create(false);
  FOriginalToItem:=TStringHashList.Create(true);
end;

constructor TPOFile.Create(const AFilename: String; Full:boolean=False);
var
  f: TStream;
begin
  f := TFileStream.Create(AFilename, fmOpenRead);
  try
    Self.Create(f, Full);
    if FHeader=nil then
      CreateHeader;
  finally
    f.Free;
  end;
end;

constructor TPOFile.Create(AStream: TStream; Full:boolean=false);
var
  Size: Integer;
  s: string;
begin
  Self.Create;
  
  FAllEntries := Full;
  
  Size:=AStream.Size-AStream.Position;
  if Size<=0 then exit;
  SetLength(s,Size);
  AStream.Read(s[1],Size);
  ReadPOText(s);
end;

destructor TPOFile.Destroy;
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
  FIdentVarToItem.Free;
  FIdentifierToItem.Free;
  FOriginalToItem.Free;
  inherited Destroy;
end;

procedure TPOFile.ReadPOText(const s: string);
{ Read a .po file. Structure:

Example
#: lazarusidestrconsts:lisdonotshowsplashscreen
msgid "                      Do not show splash screen"
msgstr ""

}
const
  sCommentIdentifier: PChar = '#: ';
  sCharSetIdentifier: PChar = '"Content-Type: text/plain; charset=';
  sMsgID: PChar = 'msgid "';
  sMsgStr: PChar = 'msgstr "';
  sMsgCtxt: Pchar = 'msgctxt "';
  
var
  l: Integer;
  LineLen: Integer;
  p: PChar;
  LineStart: PChar;
  LineEnd: PChar;
  Identifier: String;
  MsgID: String;
  Line: String;
  Comments: String;
  Context: string;
  TextEnd: PChar;
  i: Integer;
  
  procedure ResetVars;
  begin
    MsgId  := '';
    Line := '';
    Identifier := '';
    Comments := '';
    Context := '';
  end;
  
  procedure AddEntry;
  var
    Item: TPOFileItem;
  begin
    if Identifier<>'' then begin
      // check for unresolved duplicates in po file
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
        if Line='' then
          Line := Item.Translation;
      end;
      Add(Identifier,MsgID,Line,Comments,Context);
      ResetVars;
    end else
    if (Line<>'') and (FHeader=nil) then begin
      FHeader := TPOFileItem.Create('',MsgID,Line);
      FHeader.Comments:=Comments;
      ResetVars;
    end
  end;

begin
  if s='' then exit;
  l:=length(s);
  p:=PChar(s);
  LineStart:=p;
  TextEnd:=p+l;
  Identifier:='';
  Comments:='';
  Line:='';
  while LineStart<TextEnd do begin
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
      end else if CompareMem(LineStart,sMsgID,7) then begin
        // start collecting MsgId lines
        Line:=UTF8CStringToUTF8String(LineStart+7,LineLen-8);
      end else if CompareMem(LineStart,sMsgStr,8) then begin
        // store collected strings in MsgId
        MsgId := Line;
        // start collecting MsgStr lines
        Line:=UTF8CStringToUTF8String(LineStart+8,LineLen-9);
      end else if CompareMem(LineStart, sMsgCtxt,9) then begin
        Context:= Copy(LineStart, 10, LineLen-10);
      end else if LineStart^='"' then begin
        if CompareMem(LineStart,sCharSetIdentifier,35) then
          FCharSet:=copy(LineStart, 35,LineLen-37);
        Line := Line + UTF8CStringToUTF8String(LineStart+1,LineLen-2);
      end else if LineStart^='#' then begin
        if Comments<>'' then
          Comments := Comments + LineEnding;
        Comments := Comments + Copy(LineStart, 1, LineLen);
      end else
        AddEntry;
    end;
    LineStart:=LineEnd+1;
    while (LineStart<TextEnd) and (LineStart^ in [#10,#13]) do inc(LineStart);
  end;
  AddEntry;
end;

procedure TPOFile.Add(const Identifier, OriginalValue, TranslatedValue,
  Comments, Context: string);
var
  Item: TPOFileItem;
  p: Integer;
begin
  if (not FAllEntries) and (TranslatedValue='') then exit;
  //debugln('TPOFile.Add Identifier="',Identifier,'" OriginalValue="',OriginalValue,'" TranslatedValue="',TranslatedValue,'"');
  Item:=TPOFileItem.Create(Identifier,OriginalValue,TranslatedValue);
  Item.Comments:=Comments;
  Item.Context:=Context;
  Item.Tag:=FTag;
  FItems.Add(Item);
  
  FIdentifierToItem.Add(Identifier,Item);
  P := Pos('.', Identifier);
  if P>0 then
    FIdentVarToItem.Add(copy(Identifier, P+1, Length(IDentifier)), Item);
  
  //if FIdentifierToItem.Data[UpperCase(Identifier)]=nil then raise Exception.Create('');
  FOriginalToItem.Add(OriginalValue,Item);
  //if FOriginalToItem.Data[OriginalValue]=nil then raise Exception.Create('');
end;

function TPOFile.Translate(const Identifier, OriginalValue: String): String;
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

procedure TPOFile.Report;
var
  Item: TPOFileItem;
  i: Integer;
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
  
  DebugLn('Entries:');
  DebugLn('---------------------------------------------');
  for i:=0 to FItems.Count-1 do begin
    DebugLn('#',dbgs(i),': ');
    Item := TPOFileItem(FItems[i]);
    DebugLn('Comments=',Item.Comments);
    DebugLn('Identifier=',Item.Identifier);
    DebugLn('msgid=',Item.Original);
    DebugLn('msgstr=', Item.Translation);
    DebugLn;
  end;

end;

procedure TPOFile.CreateHeader;
begin
  if FHeader=nil then
    FHeader := TPOFileItem.Create('','','');
  FHeader.Translation:='Content-Type: text/plain; charset=UTF-8';
  FHeader.Comments:='';
end;

procedure TPOFile.UpdateStrings(InputLines: TStrings; SType: TStringsType);
var
  i,j,n: integer;
  p: LongInt;
  Identifier, Value,Line,UStr: string;
  Multi: boolean;
  
begin
  ClearModuleList;
  UntagAll;
  // for each string in lrt/rst list check if it's already
  // in PO if not add it
  for i:=0 to InputLines.Count-1 do begin
    Line:=InputLines[i];
    n := Length(Line);
    if n=0 then
      continue;

    if SType=stLrt then begin
      p:=Pos('=',Line);
      Value :=copy(Line,p+1,n-p); //if p=0, that's OK, all the string
      Identifier:=copy(Line,1,p-1);
      UpdateItem(Identifier, Value);
      continue;
    end;

    if (Line[1]='#') then begin
      Value := '';
      Identifier := '';
      continue;
    end;

    if Identifier='' then begin
      p:=Pos('=',Line);
      if P=0 then
        continue;
      Identifier := copy(Line,1,p-1);
      inc(p); // points to ' after =
    end else
      p:=1;   // first char in line

    // this will assume rst file is well formed and
    // do similar to rstconv but recognize utf-8 strings
    Multi := Line[n]='+';
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
        // collect a string with special chars
        UStr:='';
        repeat
          inc(p);
          j:=p;
          while (p<=n)and(Line[p] in ['0'..'9']) do
            inc(p);
          UStr := UStr + Chr(StrToInt(copy(Line, j, p-j)));
        until (Line[p]<>'#') or (p>=n);
        // transfer valid UTF-8 segments to result string
        // and re-encode back the rest
        while Ustr<>'' do begin
          j := UTF8CharacterLength(pchar(Ustr));
          if (j=1) and (Ustr[1] in [#0..#9,#11,#12,#14..#31,#128..#255]) then
            Value := Value + '#'+IntToStr(ord(Ustr[1]))
          else
            Value := Value + copy(Ustr, 1, j);
          Delete(UStr, 1, j);
        end;
      end else
      if Line[p]='+' then
        break
      else
        inc(p); // this is an unexpected string
        
    end;
    if not Multi then begin
      if Value<>'' then
        UpdateItem(Identifier, Value);
    end;
  end;
  
  RemoveUntaggedModules;
end;

procedure TPOFile.RemoveTaggedItems(aTag: Integer);
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
    //FOriginalToItem.Remove(Item.Original); // isn't this tricky?
    FItems.Delete(i);
    Item.Free;
  end;
end;

function ComparePOItems(Item1, Item2: Pointer): Integer;
begin
  result := CompareText(TPOFileItem(Item1).Identifier,
                        TPOFileItem(Item2).Identifier);
end;

procedure TPOFile.SaveToFile(const AFilename: string);
var
  OutLst: TStringList;
  j: Integer;

  procedure WriteLst(const AProp, AValue: string );
  var
    i: Integer;
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
      for i:=0 to FHelperList.Count-1 do
        if AProp='' then OutLst.Add(FHelperList[i])
        else             OutLst.Add('"'+FHelperList[i]+'\n"');
    end;
  end;
  
  procedure WriteItem(Item: TPOFileItem);
  begin
    WriteLst('',Item.Comments);
    if Item.Identifier<>'' then
      OutLst.Add('#: '+Item.Identifier);
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
      
    //if not DirectoryExists(ExtractFileDir(AFilename)) then
    //  ForceDirectories(ExtractFileDir(AFilename));
      
    OutLst.SaveToFile(AFilename);
    
  finally
    OutLst.Free;
  end;
  
end;

procedure TPOFile.UpdateItem(const Identifier: string; Original: string);
var
  Item: TPOFileItem;
  p: Integer;
  AContext,AComment,ATranslation: string;
begin
  if FHelperList=nil then
    FHelperList := TStringList.Create;

  FHelperList.Text:=Original;
  Original := FHelperList.Text; // this should unify line endings

  // try to find PO entry by identifier
  Item:=TPOFileItem(FIdentifierToItem.Data[Identifier]);
  if Item<>nil then begin
    // found, update item value
    AddToModuleList(IDentifier);
    FModified := FModified or (Item.Original<>Original);
    Item.Original:=Original;
    Item.Tag:=FTag;
    exit;
  end;

  // try to find PO entry using only variable part identifier
  p := pos('.', Identifier);
  if p>0 then begin
    Item := TPOFileItem(FIdentVarToItem.Data[RightStr(Identifier, Length(Identifier)-P)]);
    if Item<>nil then begin
      // found!, this means module name has changed
      AddToModuleList(Item.Identifier);
      // update identifier list
      FIdentifierToItem.Remove(Item.Identifier);
      FIdentifierToItem.Add(Identifier, Item);
      // update item
      FModified := true;
      Item.Identifier:=Identifier;
      Item.Original:=Original;
      Item.Tag := FTag;
      exit;
    end;
  end;
  
  // try to find po entry based only on it's value
  AContext := '';
  AComment := '';
  ATranslation := '';
  Item := TPOFileItem(FOriginalToItem.Data[Original]);
  if Item<>nil then begin
    // old item don't have context, add one
    if Item.Context='' then
      Item.Context := Item.Identifier;
      
    // if old item it's already translated use translated
    if Item.Translation<>'' then
      ATranslation := Item.Translation;
    
    // update identifier list
    AContext := Identifier;
  end;

  // this appear to be a new item
  FModified := true;
  Add(Identifier, Original, ATranslation, AComment, AContext);
end;

procedure TPOFile.UpdateTranslation(BasePOFile: TPOFile);
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

procedure TPOFile.ClearModuleList;
begin
  if FModuleList<>nil then
    FModuleList.Clear;
end;

procedure TPOFile.AddToModuleList(Identifier: string);
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

procedure TPOFile.UntagAll;
var
  Item: TPOFileItem;
  i: Integer;
begin
  for i:=0 to Items.Count-1 do begin
    Item := TPOFileItem(Items[i]);
    Item.Tag:=0;
  end;
end;

{ TPOFileItem }

constructor TPOFileItem.Create(const TheIdentifier, TheOriginal,
  TheTranslated: string);
begin
  Identifier:=TheIdentifier;
  Original:=TheOriginal;
  Translation:=TheTranslated;
end;

end.


