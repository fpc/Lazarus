unit RecentListProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg, LazUTF8, LazFileUtils, StdCtrls,
  IdeXmlConfigProcs;

// Recent item lists
type
  TRecentListType = (
    rltCaseSensitive,
    rltCaseInsensitive,
    rltFile
    );
const
  RecentListTypeNames: array[TRecentListType] of string = (
    'CaseSensitive',
    'CaseInsensitive',
    'File'
    );
function IndexInRecentList(List: TStrings; ListType: TRecentListType;
  const Path: string): integer;
function StrToRecentListType(s: string): TRecentListType;
function CompareRecentListItem(s1, s2: string; ListType: TRecentListType): boolean;
procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStrings; const Path: string;
                         ListType: TRecentListType);
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings;
                         const Path: string); overload;
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings;
                         const Path: string; aMax: Integer); overload;
function AddToRecentList(const s: string; List: TStrings; aMax: integer;
                         ListType: TRecentListType): boolean;
function AddComboTextToRecentList(cb: TComboBox; aMax: integer;
                                  ListType: TRecentListType): boolean;
procedure RemoveFromRecentList(const s: string; List: TStrings;
                               ListType: TRecentListType);
procedure CleanUpRecentList(List: TStrings; ListType: TRecentListType);

implementation

// Recent item lists :

function IndexInRecentList(List: TStrings; ListType: TRecentListType;
  const Path: string): integer;
begin
  Result:=List.Count-1;
  while (Result>=0) and (not CompareRecentListItem(List[Result],Path,ListType)) do
    dec(Result);
end;

function StrToRecentListType(s: string): TRecentListType;
begin
  for Result:=Low(TRecentListType) to high(TRecentListType)  do
    if SysUtils.CompareText(s,RecentListTypeNames[Result])=0 then exit;
  Result:=rltCaseSensitive;
end;

function CompareRecentListItem(s1, s2: string; ListType: TRecentListType): boolean;
begin
  case ListType of
  rltCaseInsensitive: Result:=UTF8CompareLatinTextFast(s1,s2)=0;
  rltFile: Result:=CompareFilenames(ChompPathDelim(s1),ChompPathDelim(s2))=0;
  else Result:=s1=s2;
  end;
end;

procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStrings;
  const Path: string; ListType: TRecentListType);
begin
  LoadStringList(XMLConfig,List,Path);
  CleanUpRecentList(List,ListType);
end;

procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
begin
  SaveStringList(XMLConfig,List,Path);
end;

procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings;
  const Path: string; aMax: Integer);
var
  i: Integer;
  s: String;
begin
  if aMax>0 then
    while List.Count>aMax do    // Truncate list to aMax items.
      List.Delete(List.Count-1);
  SaveStringList(XMLConfig,List,Path);
  i:=List.Count+1;
  while True do
  begin
    s:=Path+'Item'+IntToStr(i);
    if not XMLConfig.HasPath(s+'/Value',True) then Break;
    XMLConfig.DeletePath(s);    // Remove excess items from XML.
    Inc(i);
  end;
end;

function AddToRecentList(const s: string; List: TStrings; aMax: integer;
  ListType: TRecentListType): boolean;
begin
  if (List.Count>0) and CompareRecentListItem(List[0],s,ListType) then
    exit(false);
  Result:=true;
  RemoveFromRecentList(s,List,ListType);
  List.Insert(0,s);
  if aMax>0 then
    while List.Count>aMax do
      List.Delete(List.Count-1);
end;

function AddComboTextToRecentList(cb: TCombobox; aMax: integer;
  ListType: TRecentListType): boolean;
var
  List: TStringList;
begin
  List:=TStringList.Create;
  try
    List.Assign(cb.Items);
    Result:=AddToRecentList(cb.Text,List,aMax,ListType);
    if Result then
    begin
      cb.Items.Assign(List);
      cb.ItemIndex:=0;
    end;
  finally
    List.Free;
  end;
end;

procedure RemoveFromRecentList(const s: string; List: TStrings;
  ListType: TRecentListType);
var
  i: integer;
begin
  for i:=List.Count-1 downto 0 do
    if CompareRecentListItem(List[i],s,ListType) then
      List.Delete(i);
end;

procedure CleanUpRecentList(List: TStrings; ListType: TRecentListType);
var
  i: Integer;
begin
  for i:=List.Count-1 downto 1 do
    if (List[i]='') or CompareRecentListItem(List[i],List[i-1],ListType) then
      List.Delete(i);
end;

end.

