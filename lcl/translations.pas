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
    of the gettext unit in the project lpr file:

    uses
      ...
      Translations, LCLProc;

    procedure TranslateLCL;
    var
      PODirectory, Lang, FallbackLang: String;
    begin
      PODirectory:='/path/to/lazarus/lcl/languages/';
      Lang:='';
      FallbackLang:='';
      LCLGetLanguageIDs(Lang,FallbackLang); // in unit LCLProc
      Translations.TranslateUnitResourceStrings('LCLStrConsts',
                                PODirectory+'lclstrconsts.%s.po',Lang,FallbackLang);
    end;

    begin
      TranslateLCL;
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end.

    Note for Mac OS X:
      The supported language IDs should be added into the application
      bundle property list to CFBundleLocalizations key, see
      lazarus.app/Contents/Info.plist for example.
}
unit Translations;

{$mode objfpc}{$H+}{$INLINE ON}
{$include include/lcl_defines.inc}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, StringHashList, AvgLvlTree,
  LConvEncoding;

type
  TStringsType = (stLrt, stRst);
  TTranslateUnitResult = (turOK, turNoLang, turNoFBLang, turEmptyParam);

type
  { TPOFileItem }

  TPOFileItem = class
  public
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

  { TPOFile }

  TPOFile = class
  protected
    FItems: TFPList;// list of TPOFileItem
    FIdentifierToItem: TStringToPointerTree; // of TPOFileItem
    FIdentVarToItem: TStringHashList; // of TPOFileItem
    FOriginalToItem: TStringHashList; // of TPOFileItem
    FCharSet: String;
    FHeader: TPOFileItem;
    FAllEntries: boolean;
    FTag: Integer;
    FModified: boolean;
    FHelperList: TStringList;
    FModuleList: TStringList;
    procedure RemoveTaggedItems(aTag: Integer);
    procedure RemoveUntaggedModules;
    function IsKey(Txt, Key: PChar): boolean;
  public
    constructor Create;
    constructor Create(const AFilename: String; Full:boolean=false);
    constructor Create(AStream: TStream; Full:boolean=false);
    destructor Destroy; override;
    procedure ReadPOText(const Txt: string);
    procedure Add(const Identifier, OriginalValue, TranslatedValue, Comments,
                        Context, Flags, PreviousID: string);
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
function TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string):TTranslateUnitResult; overload;
function TranslateUnitResourceStrings(const ResUnitName, AFilename: string
  ): boolean; overload;
function TranslateUnitResourceStrings(const ResUnitName:string; po: TPOFile): boolean; overload;

// translate all resource strings
function TranslateResourceStrings(po: TPOFile): boolean;
function TranslateResourceStrings(const AFilename: string): boolean;
procedure TranslateResourceStrings(const BaseFilename, Lang, FallbackLang: string);

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
  if FindFirstUTF8(Path+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      or (CompareFilenames(FileInfo.Name,Name)=0) then continue;
      CurExt:=ExtractFileExt(FileInfo.Name);
      if (CompareFilenames(CurExt,'.po')<>0)
      or (CompareFilenames(LeftStr(FileInfo.Name,length(NameOnly)),NameOnly)<>0)
      then
        continue;
      Result.Add(Path+FileInfo.Name);
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

function UpdatePOFile(Files: TStrings; const POFilename: string): boolean;
var
  InputLines: TStringList;
  Filename: string;
  BasePoFile, POFile: TPoFile;
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
        POFile := TPOFile.Create(Lines[j], true);
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
      BasePOFile := TPOFile.Create(POFilename, true);
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
      BasePOFile := TPOFile.Create(POFilename, true)
    else
      BasePOFile := TPOFile.Create;
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

function TranslateUnitResourceStrings(const ResUnitName, AFilename: string
  ): boolean;
var po: TPOFile;
begin
  //debugln('TranslateUnitResourceStrings) ResUnitName="',ResUnitName,'" AFilename="',AFilename,'"');
  if (ResUnitName='') or (AFilename='') or (not FileExistsUTF8(AFilename)) then
    exit;
  result:=false;
  po:=nil;
  try
    po:=TPOFile.Create(AFilename);
    result:=TranslateUnitResourceStrings(ResUnitName,po);
  finally
    po.free;
  end;
end;

function TranslateUnitResourceStrings(const ResUnitName: string; po: TPOFile): boolean;
begin
  Result:=false;
  try
    SetUnitResourceStrings(ResUnitName,@Translate,po);
    Result:=true;
  except
    on e: Exception do begin
      {$IFNDEF DisableChecks}
      DebugLn('Exception while translating ', ResUnitName);
      DebugLn(e.Message);
      DumpExceptionBackTrace;
      {$ENDIF}
    end;
  end;
end;

function TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string):TTranslateUnitResult;
begin
  Result:=turOK;                //Result: OK
  if (ResUnitName='') or (BaseFilename='') then
    Result:=turEmptyParam       //Result: empty Parameter
  else begin
    //debugln('TranslateUnitResourceStrings BaseFilename="',BaseFilename,'"');
    if (FallbackLang<>'') and FileExistsUTF8(Format(BaseFilename,[FallbackLang])) then
      TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[FallbackLang]))
    else
      Result:=turNoFBLang;      //Result: missing FallbackLang file
    if (Lang<>'') and FileExistsUTF8(Format(BaseFilename,[Lang])) then
      TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[Lang]))
    else
      Result:=turNoLang;        //Result: missing Lang file
  end;
end;

function TranslateResourceStrings(po: TPOFile): boolean;
begin
  Result:=false;
  try
    SetResourceStrings(@Translate,po);
    Result:=true;
  except
    on e: Exception do begin
      {$IFNDEF DisableChecks}
      DebugLn('Exception while translating:');
      DebugLn(e.Message);
      DumpExceptionBackTrace;
      {$ENDIF}
    end;
  end;
end;

function TranslateResourceStrings(const AFilename: string): boolean;
var po: TPOFile;
begin
  //debugln('TranslateResourceStrings) ResUnitName,'" AFilename="',AFilename,'"');
  if (AFilename='') or (not FileExistsUTF8(AFilename)) then
    exit;
  result:=false;
  po:=nil;
  try
    po:=TPOFile.Create(AFilename);
    result:=TranslateResourceStrings(po);
  finally
    po.free;
  end;
end;

procedure TranslateResourceStrings(const BaseFilename, Lang, FallbackLang: string);
begin
  if (BaseFilename='') then exit;

  //debugln('TranslateResourceStrings BaseFilename="',BaseFilename,'"');
  if (FallbackLang<>'') then
    TranslateResourceStrings(Format(BaseFilename,[FallbackLang]));
  if (Lang<>'') then
    TranslateResourceStrings(Format(BaseFilename,[Lang]));
end;

{ TPOFile }

procedure TPOFile.RemoveUntaggedModules;
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

    //FOriginalToItem.Remove(Item.Original); // isn't this tricky?
    FItems.Delete(i);
    Item.Free;
  end;
end;

function TPOFile.IsKey(Txt, Key: PChar): boolean;
begin
  if Txt=nil then exit(false);
  if Key=nil then exit(true);
  repeat
    if Key^=#0 then exit(true);
    if Txt^<>Key^ then exit(false);
    inc(Key);
    inc(Txt);
  until false;
end;

constructor TPOFile.Create;
begin
  inherited Create;
  FAllEntries:=true;
  FItems:=TFPList.Create;
  FIdentifierToItem:=TStringToPointerTree.Create(false);
  FIdentVarToItem:=TStringHashList.Create(false);
  FOriginalToItem:=TStringHashList.Create(true);
end;

constructor TPOFile.Create(const AFilename: String; Full:boolean=False);
var
  f: TStream;
begin
  f := TFileStream.Create(UTF8ToSys(AFilename), fmOpenRead or fmShareDenyNone);
  try
    Create(f, Full);
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
  Create;
  
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

procedure TPOFile.ReadPOText(const Txt: string);
{ Read a .po file. Structure:

Example
#: lazarusidestrconsts:lisdonotshowsplashscreen
msgid "                      Do not show splash screen"
msgstr ""

}
const
  ciNone      = 0;
  ciMsgID     = 1;
  ciMsgStr    = 2;
  ciPrevMsgID = 3;
  
var
  l: Integer;
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
  Handled: Boolean;
  
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
      Add(Identifier,MsgID,MsgStr,Comments,Context,Flags,PrevMsgID);
      ResetVars;
    end else
    if (Line<>'') and (FHeader=nil) then begin
      FHeader := TPOFileItem.Create('',MsgID,Line);
      FHeader.Comments:=Comments;
      ResetVars;
    end
  end;

  procedure StartNextLine(AIndex: Integer; TxtStart: PChar);
  begin
    StoreCollectedLine;
    CollectedIndex := AIndex;
    Line:=UTF8CStringToUTF8String(TxtStart,LineEnd-TxtStart-1); // -1 for the ending "
  end;

begin
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

  while LineStart<TextEnd do begin
    LineEnd:=LineStart;
    while (not (LineEnd^ in [#0,#10,#13])) do inc(LineEnd);
    LineLen:=LineEnd-LineStart;
    if LineLen>0 then begin
      Handled:=false;
      case LineStart^ of
      '#':
        begin
          case LineStart[1] of
          ':':
            if LineStart[2]=' ' then begin
              // '#: '
              AddEntry;
              Identifier:=copy(s,LineStart-p+4,LineLen-3);
              // the RTL creates identifier paths with point instead of colons
              // fix it:
              for i:=1 to length(Identifier) do
                if Identifier[i]=':' then
                  Identifier[i]:='.';
              Handled:=true;
            end;
          '|':
            if IsKey(LineStart,'#| msgid "') then begin
              StartNextLine(ciPrevMsgId,LineStart+length('#| msgid "'));
              Handled:=true;
            end else if IsKey(LineStart, '#| "') then begin
              Line := Line + UTF8CStringToUTF8String(LineStart+5,LineLen-6);
              Handled:=true;
            end;
          ',':
            if LineStart[2]=' ' then begin
              // '#, '
              Flags := copy(LineStart, 4, LineLen-3);
              Handled:=true;
            end;
          end;
          if not Handled then begin
            // '#'
            if Comments<>'' then
              Comments := Comments + LineEnding;
            Comments := Comments + Copy(LineStart, 1, LineLen);
            Handled:=true;
          end;
        end;
      'm':
        if IsKey(LineStart,'msgid "') then begin
          StartNextLine(ciMsgId,LineStart+length('msgid "'));
          Handled:=true;
        end else if IsKey(LineStart,'msgstr "') then begin
          StartNextLine(ciMsgStr,LineStart+length('msgstr "'));
          Handled:=true;
        end else if IsKey(LineStart, 'msgctxt "') then begin
          Context:= Copy(LineStart, 10, LineLen-10);
          Handled:=true;
        end;
      '"':
        begin
          if (MsgID='')
          and IsKey(LineStart,'"Content-Type: text/plain; charset=') then
          begin
            FCharSet:=copy(LineStart,36,LineLen-38);
            if SysUtils.CompareText(FCharSet,'UTF-8')<>0 then begin
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
          end;
          Line := Line + UTF8CStringToUTF8String(LineStart+1,LineLen-2);
          Handled:=true;
        end;
      end;
      if not Handled then
        AddEntry;
    end;
    LineStart:=LineEnd+1;
    while (LineStart<TextEnd) and (LineStart^ in [#10,#13]) do inc(LineStart);
  end;
  AddEntry;
end;

procedure TPOFile.Add(const Identifier, OriginalValue, TranslatedValue,
  Comments, Context, Flags, PreviousID: string);
var
  Item: TPOFileItem;
  p: Integer;
begin
  if (not FAllEntries) and (TranslatedValue='') then exit;
  Item:=TPOFileItem.Create(Identifier,OriginalValue,TranslatedValue);
  Item.Comments:=Comments;
  Item.Context:=Context;
  Item.Flags:=Flags;
  Item.PreviousID:=PreviousID;
  Item.Tag:=FTag;
  FItems.Add(Item);

  //debugln(['TPOFile.Add Identifier=',Identifier,' Orig="',dbgstr(OriginalValue),'" Transl="',dbgstr(TranslatedValue),'"']);
  FIdentifierToItem[Identifier]:=Item;
  P := Pos('.', Identifier);
  if P>0 then
    FIdentVarToItem.Add(copy(Identifier, P+1, Length(IDentifier)), Item);
  
  FOriginalToItem.Add(OriginalValue,Item);
end;

function TPOFile.Translate(const Identifier, OriginalValue: String): String;
var
  Item: TPOFileItem;
begin
  Item:=TPOFileItem(FIdentifierToItem[Identifier]);
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

procedure TPOFile.UpdateItem(const Identifier: string; Original: string);
var
  Item: TPOFileItem;
  AContext,AComment,ATranslation,AFlags,APrevStr: string;
begin
  if FHelperList=nil then
    FHelperList := TStringList.Create;

  // try to find PO entry by identifier
  Item:=TPOFileItem(FIdentifierToItem[Identifier]);
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


