unit PotFile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazFileUtils, FileUtil, Contnrs;

type

  { TTranslationSection }

  TTranslationSection = class(TStringList)
  private
    FOwner: TObject;
    FVarName, FMsgId, FMsgStr: String;
    function GetVarName: String;
    function GetMsgId: String;
    function GetMsgStr: String;
    function GetMsgIdContent: String;
    function GetMsgIdCleanContent: String;
    function GetMsgStrContent: String;
    function GetMsgStrCleanContent: String;
    function GetVarNameContent: String;
    function GetVarNameIdent: String;
    function GetVarNameUnit: String;
    procedure SetMsgStrContent(AValue: String);

    function FindLineIdx(AName: String): Integer;
    function FindLine(AName: String): String;
    function CleanLineContent(AText: String): String;
  protected
    procedure SetTextStr(const Value: string); override;
    function CreateCopy: TTranslationSection;
    property Owner: TObject read FOwner;
  public
    procedure ReplaceUnitPrefix(NewPrefix: String);
    procedure DeleteMsgCtxt;
    function IsEmpty: boolean;

    property VarName: String read GetVarName;
    property VarNameContent: String read GetVarNameContent;
    property VarNameIdent: String read GetVarNameIdent;
    property VarNameUnit: String read GetVarNameUnit;
    property MsgId: String read GetMsgId;
    property MsgIdContent: String read GetMsgIdContent;
    property MsgIdCleanContent: String read GetMsgIdCleanContent;
    property MsgStr: String read GetMsgStr;
    property MsgStrContent: String read GetMsgStrContent write SetMsgStrContent;
    property MsgStrCleanContent: String read GetMsgStrCleanContent;
  end;
  TTranslationSectionClass = class of TTranslationSection;

  { TTranslationList }

  generic TTranslationList<TBase: TTranslationSection> = class(specialize TFPGObjectList<TBase>)
  private
    FFileName: String;
    FModified: Boolean;
    FName: String;
  protected
    function CreateEntry: TTranslationSection; virtual; abstract;
  public
    constructor Create(AFileName: String; ACreateEmpty: boolean = False);
    function Add(const Item: TBase): Integer; inline;

    procedure Load; virtual;
    procedure Save; virtual;
    procedure SetModified; virtual;

    function IndexOfVar(AVarName: String): integer;
    function IndexOfVarContent(AVarName: String): integer;
    function IsEmpty: boolean; // all MsgStr = ''
    function NoneEmptyCount: Integer; // all MsgStr = ''
    property FileName: String read FFileName;
    property Name: String read FName;
    property Modified: Boolean read FModified;
  end;

  { TPoSection }

  TPoSection = class(TTranslationSection)
  public
    function CreateCopy: TPoSection;
  end;

  { TPoFile }

  TPoFile = class(specialize TTranslationList<TPoSection>)
  private
    FLangName: String;
  protected
    function CreateEntry: TTranslationSection; override;
  public
    constructor Create(AFileName: String; ACreateEmpty: boolean = False);
    property LangName: String read FLangName;
  end;

  TPotFile = class;

  { TPoFileList }

  TPoFileList = class(specialize TFPGObjectList<TPoFile>)
  private
    FOwner: TPotFile;
  public
    constructor Create(AOwner: TPotFile);
    procedure Clear;
    function PoFileForLang(ALang: String; ACreate: Boolean = False): TPoFile;
  end;

  { TPotSection }

  TPotSection = class(TTranslationSection)
  private
    function GetOwner: TPotFile; reintroduce;
  public
    function CreateCopy: TPotSection;
    function TranslationCount: integer;
    function TranslationNoneEmptyCount: integer;
    procedure GetTranslationyCount(out TotalCnt, NoneEmptyCnt: integer);
    property Owner: TPotFile read GetOwner;
  end;

  { TPotFile }

  TPotFile = class(specialize TTranslationList<TPotSection>)
  private
    FPoFiles: TPoFileList;
  protected
    function CreateEntry: TTranslationSection; override;
    procedure LoadPoFiles;
    procedure SavePoFiles;
  public
    constructor Create(AFileName: String; ACreateEmpty: boolean = False);
    destructor Destroy; override;
    procedure Clear;
    procedure Load; override;
    procedure Save; override;

    property PoFiles: TPoFileList read FPoFiles;
  end;

  TFindDupFlag = (fdIgnoreCase, fdIgnoreSpaceDiff);
  TFindDupFlags = set of TFindDupFlag;

  { TPotFileList }

  TPotFileList = class(specialize TFPGObjectList<TPotFile>)
  public
    procedure FindDuplicateMsgId(ARes: TPotFile; AFlags: TFindDupFlags = []);
  end;

implementation

{ TPoSection }

function TPoSection.CreateCopy: TPoSection;
begin
  Result := TPoSection(inherited CreateCopy);
end;

{ TPotSection }

function TPotSection.GetOwner: TPotFile;
begin
  Result := TPotFile(inherited Owner);
end;

function TPotSection.CreateCopy: TPotSection;
begin
  Result := TPotSection(inherited CreateCopy);
end;

function TPotSection.TranslationCount: integer;
var
  po: TPoFile;
  i: Integer;
begin
  Result := 0;
  if Owner = nil then
    exit;
  for i := 0 to Owner.PoFiles.Count - 1 do begin
    po := Owner.PoFiles[i];
    if po.IndexOfVar(VarName) >= 0 then
      inc(Result);
  end;
end;

function TPotSection.TranslationNoneEmptyCount: integer;
var
  po: TPoFile;
  i, e: Integer;
begin
  Result := 0;
  if Owner = nil then
    exit;
  for i := 0 to Owner.PoFiles.Count - 1 do begin
    po := Owner.PoFiles[i];
    e := po.IndexOfVar(VarName);
    if (e  >= 0) and (not po[e].IsEmpty) then
      inc(Result);
  end;
end;

procedure TPotSection.GetTranslationyCount(out TotalCnt, NoneEmptyCnt: integer);
var
  po: TPoFile;
  i, e: Integer;
begin
  TotalCnt := 0;
  NoneEmptyCnt := 0;
  if Owner = nil then
    exit;
  for i := 0 to Owner.PoFiles.Count - 1 do begin
    po := Owner.PoFiles[i];
    e := po.IndexOfVar(VarName);
    if (e  >= 0) then begin
      inc(TotalCnt);
      if (not po[e].IsEmpty) then
        inc(NoneEmptyCnt);
    end;
  end;
end;

{ TTranslationSection }

function TTranslationSection.GetVarName: String;
begin
  if FVarName = '' then
    FVarName := FindLine('#: ');

  Result := FVarName;
end;

function TTranslationSection.GetMsgStrContent: String;
begin
  Result := MsgStr;
  system.Delete(Result, 1, 7);
end;

function TTranslationSection.GetMsgId: String;
begin
  if FMsgId = '' then
    FMsgId := FindLine('msgid ');

  Result := FMsgId;
end;

function TTranslationSection.GetMsgStr: String;
begin
  if FMsgStr = '' then
    FMsgStr := FindLine('msgstr ');

  Result := FMsgStr;
end;

function TTranslationSection.GetMsgIdContent: String;
begin
  Result := MsgId;
  system.Delete(Result, 1, 6);
end;

function TTranslationSection.GetMsgIdCleanContent: String;
begin
  Result := CleanLineContent(MsgIdContent);
end;

function TTranslationSection.GetMsgStrCleanContent: String;
begin
  Result := CleanLineContent(MsgStrContent);
end;

function TTranslationSection.GetVarNameContent: String;
begin
  Result := VarName;
  system.Delete(Result, 1, 3);
end;

function TTranslationSection.GetVarNameIdent: String;
var
  i: SizeInt;
begin
  Result := VarName;

  i := Length(Result);
  while (i > 0) and (Result[i] <> '.') do
    dec(i);
  if i > 0 then
    system.Delete(Result, 1, i);
end;

function TTranslationSection.GetVarNameUnit: String;
var
  i: SizeInt;
begin
  Result := VarNameContent;

  i := Length(Result);
  while (i > 0) and (Result[i] <> '.') do
    dec(i);
  if i > 0 then
    system.Delete(Result, i, Length(Result));
end;

function TTranslationSection.FindLineIdx(AName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := Count - 1;
  while i >= 0 do begin
    if strlcomp(PChar(AName), PChar(Strings[i]), Length(AName)) = 0 then begin
      Result := i;
      exit;
    end;
    dec(i);
  end;
end;

function TTranslationSection.FindLine(AName: String): String;
var
  i: Integer;
begin
  i := FindLineIdx(AName);
  Result := '';
  if i >= 0 then
    Result := Strings[i];
end;

function TTranslationSection.CleanLineContent(AText: String): String;
var
  i: SizeInt;
begin
  Result := AText;
  if (Result <> '') and (Result[1] = '"') then
    system.Delete(Result, 1, 1);
  if (Result <> '') and (Result[Length(Result)] = '"') then
    system.Delete(Result, Length(Result), 1);

  Result := StringReplace(Result, '\"', '"', [rfReplaceAll]);
end;

procedure TTranslationSection.SetMsgStrContent(AValue: String);
var
  i: Integer;
begin
  i := FindLineIdx('msgstr ');
  if i < 0 then i := Add('');

  Strings[i] := 'msgstr "' + AValue + '"';
  FMsgStr := '';
end;

procedure TTranslationSection.SetTextStr(const Value: string);
begin
  inherited SetTextStr(Value);
  FVarName := '';
  FMsgId := '';
  FMsgStr := '';
end;

function TTranslationSection.CreateCopy: TTranslationSection;
begin
  Result := TTranslationSectionClass(ClassType).Create;
  Result.Text := Text;
end;

procedure TTranslationSection.ReplaceUnitPrefix(NewPrefix: String);
var
  Old: String;
  i: SizeInt;
begin
  Old := VarNameContent;
  i := Length(Old);
  while (i > 0) and (Old[i] <> '.') do
    dec(i);
  if i < 1 then
    exit;

  system.Delete(Old, i+1, Length(old)); // keep the dot

  for i := 0 to Count - 1 do begin
    if (Strings[i] <> '') and
       ( (Strings[i][1] = '#') or
         (strlcomp(pchar('msgctxt '), pchar(Strings[i]), 8) = 0)
       )
    then
      Strings[i] := StringReplace(Strings[i], Old, NewPrefix+'.', [rfReplaceAll]);
  end;
end;

procedure TTranslationSection.DeleteMsgCtxt;
var
  i: Integer;
begin
  i := Count - 1;
  while (i >= 0) and (strlcomp(pchar('msgctxt '), pchar(Strings[i]), 8) <> 0) do
    dec(i);
  if i >= 0 then
    Delete(i);
end;

function TTranslationSection.IsEmpty: boolean;
begin
  Result := MsgStrCleanContent = '';
end;

{ TTranslationList }

constructor TTranslationList.Create(AFileName: String; ACreateEmpty: boolean);
begin
  inherited Create(True);
  FFileName := AFileName;
  FName := ExtractFileNameOnly(AFileName);
  if not ACreateEmpty then
    Load;
end;

function TTranslationList.Add(const Item: TBase): Integer;
begin
  Item.FOwner := Self;
  Result := inherited Add(Item);
end;

procedure TTranslationList.Load;
var
  AFile: TStringList;
  i, c: Integer;
  j: LongInt;
  e: TTranslationSection;
begin
  Clear;
  AFile := TStringList.Create;
  AFile.LoadFromFile(FFileName);

  i := 0;
  c := AFile.Count;
  while i < c do begin
    if AFile[i] = '' then begin
      inc(i);
      Continue;
    end;
    j := i;
    while (i < c) and (AFile[i] <> '') do
      inc(i);
    if i > j then begin
      e := CreateEntry;
      while j < i do begin
        e.Add(AFile[j]);
        inc(j);
      end;
      Add(TBase(e));
    end;
  end;

  AFile.Free;
end;

procedure TTranslationList.Save;
var
  AFile: TStringList;
  s: String;
  i: Integer;
begin
  AFile := TStringList.Create;
  s := '';
  for i := 0 to Count - 1 do begin
    s := s + Items[i].Text + LineEnding;
  end;
  AFile.Text := s;
  AFile.SaveToFile(FFileName);
  AFile.Free;

  FModified := False;
end;

procedure TTranslationList.SetModified;
begin
  FModified := True;
end;

function TTranslationList.IndexOfVar(AVarName: String): integer;
begin
  Result := Count - 1;
  while Result >= 0 do begin
    if Items[Result].VarName = AVarName then
      exit;
    dec(Result);
  end;
end;

function TTranslationList.IndexOfVarContent(AVarName: String): integer;
begin
  Result := Count - 1;
  while Result >= 0 do begin
    if Items[Result].VarNameContent = AVarName then
      exit;
    dec(Result);
  end;
end;

function TTranslationList.IsEmpty: boolean;
var
  i: Integer;
begin
  result := False;
  for i := 1 to Count - 1 do
    if not Items[i].IsEmpty then
      exit;
  result := True;
end;

function TTranslationList.NoneEmptyCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count - 1 do
    if not Items[i].IsEmpty then
      inc(Result);
end;

{ TPoFile }

function TPoFile.CreateEntry: TTranslationSection;
begin
  Result := TPoSection.Create;
end;

constructor TPoFile.Create(AFileName: String; ACreateEmpty: boolean);
var
  i: SizeInt;
begin
  inherited Create(AFileName, ACreateEmpty);
  FLangName := Name;
  i := Length(FLangName);
  while (i > 0) and (FLangName[i] <> '.') do
    dec(i);
  if i > 0 then
    system.Delete(FLangName, 1, i);
end;

{ TPoFileList }

constructor TPoFileList.Create(AOwner: TPotFile);
begin
  FOwner := AOwner;
  inherited Create(True);
end;

procedure TPoFileList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Clear;
  inherited Clear;
end;

function TPoFileList.PoFileForLang(ALang: String; ACreate: Boolean): TPoFile;
var
  i: Integer;
  h: TPoSection;
begin
  i := Count - 1;
  while i >= 0 do begin
    if Items[i].LangName = ALang then
      exit(Items[i]);
    dec(i);
  end;

  Result := TPoFile.Create(ExtractFileNameWithoutExt(FOwner.FileName) + '.' + ALang + '.po', True);
  h := TPoSection.Create;
  h.Text :=
      'msgid ""' + LineEnding +
      'msgstr ""' + LineEnding +
      '"Project-Id-Version: \n"' + LineEnding +
      '"MIME-Version: 1.0\n"' + LineEnding +
      '"Content-Type: text/plain; charset=utf-8\n"' + LineEnding +
      '"Content-Transfer-Encoding: 8bit\n"' + LineEnding
      ;
  Result.Add(h);
  Add(Result);
end;

{ TPotFile }

function TPotFile.CreateEntry: TTranslationSection;
begin
  Result := TPotSection.Create;
end;

procedure TPotFile.LoadPoFiles;
var
  ADir: String;
  AList: TStringList;
  i: Integer;
begin
  ADir := AppendPathDelim(ExtractFileDir(FFileName));
  AList := FindAllFiles(ADir, FName + '.*.po', False, faAnyFile);
  if (AList = nil) then
    exit;

  for i := 0 to AList.Count - 1 do
    FPoFiles.Add( TPoFile.Create(AList[i]) );

  AList.Free;
end;

procedure TPotFile.SavePoFiles;
var
  i: Integer;
begin
  for i := 0 to FPoFiles.Count - 1 do
    FPoFiles[i].Save;
end;

constructor TPotFile.Create(AFileName: String; ACreateEmpty: boolean);
begin
  FPoFiles := TPoFileList.Create(Self);
  inherited Create(AFileName, ACreateEmpty);
end;

destructor TPotFile.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FPoFiles);
end;

procedure TPotFile.Clear;
begin
  FPoFiles.Clear;
  inherited Clear;
end;

procedure TPotFile.Load;
begin
  inherited Load;
  LoadPoFiles;
end;

procedure TPotFile.Save;
begin
  inherited Save;
  SavePoFiles;
end;

{ TPotFileList }

procedure TPotFileList.FindDuplicateMsgId(ARes: TPotFile; AFlags: TFindDupFlags
  );

  function GetComparableMsgId(msgid: string): String;
  var
    i, j: Integer;
  begin
    Result := msgid;

    if fdIgnoreCase in AFlags then
      Result := LowerCase(Result);

    if fdIgnoreSpaceDiff in AFlags then begin
      Result := Trim(Result);
      i := 0;
      j := 0;
      while i < length(Result) do begin
        inc(i);
        inc(j);
        Result[j] := Result[i];
        if Result[i] in [#0..' '] then begin
          Result[j] := ' ';
          while (i < length(Result)) and (Result[i+1] in [#0..' ']) do
            inc(i);
          if ( (j > 1) and (not (Result[j-1] in ['a'..'z', 'A'..'Z', '0'..'9', '_'])) ) or
             ( (i < Length(Result)) and (not (Result[i+1] in ['a'..'z', 'A'..'Z', '0'..'9', '_'])) )
          then
            dec(j);
        end;
      end;
      SetLength(Result, j);
    end;
  end;

  procedure AddDup(ANewVarName: String; ATargetPot: TPotSection; ASrc: TPotSection);
  var
    po: TPoFile;
    poSect: TPoSection;
    e: LongInt;
    i: Integer;
    n, cmpMsgId, s: String;
  begin
    n := ASrc.Owner.Name;
    po := ARes.PoFiles.PoFileForLang(n+'  ');
    e := po.IndexOfVarContent(ANewVarName);
    i := 0;
    while e >= 0 do begin
      inc(i);
      po := ARes.PoFiles.PoFileForLang(n+'_'+IntToStr(i));
      e := po.IndexOfVarContent(ANewVarName);
    end;
    poSect := TPoSection.Create;
    poSect.Text := '#: ' + ANewVarName + LineEnding +
      'msgstr "' + ASrc.VarNameContent + ': ' + ASrc.MsgIdCleanContent + '"' + LineEnding;
    po.Add(poSect);

    ATargetPot.MsgStrContent := ATargetPot.MsgStrCleanContent + ',' + ASrc.VarNameIdent;
  end;

var
  TmpHash: TFPObjectHashTable;
  i, j: Integer;
  pot: TPotFile;
  potItm: TPotSection;
  dupPotItm, newDupPotItm: TPotSection;
  msgid: String;
begin
  ARes.Clear;

  TmpHash := TFPObjectHashTable.Create(False);

  for i := 0 to Count - 1 do begin
    pot := Items[i];
    for j := 1 to pot.Count - 1 do begin
      potItm := pot.Items[j];
      msgid := GetComparableMsgId(potItm.MsgIdCleanContent);
      dupPotItm := TPotSection(TmpHash[msgid]);
      if dupPotItm = nil then begin
        TmpHash.Add(msgid, potItm);
      end
      else
      if dupPotItm.Owner = ARes then begin
        // already a dup
        AddDup(DupPotItm.VarNameContent, dupPotItm, potItm);
      end
      else
      begin
        newDupPotItm := TPotSection.Create;
        newDupPotItm.Text := '#: ' + dupPotItm.VarNameIdent + '__' + IntToHex(PtrUInt(newDupPotItm)) + LineEnding +
          dupPotItm.MsgId + LineEnding;
        ARes.Add(newDupPotItm);
        TmpHash[msgid] := newDupPotItm;

        AddDup(newDupPotItm.VarNameContent, newDupPotItm, dupPotItm);
        AddDup(newDupPotItm.VarNameContent, newDupPotItm, potItm);
      end;
    end;
  end;



  TmpHash.Free;
end;

end.

