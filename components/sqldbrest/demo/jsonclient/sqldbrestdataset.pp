unit sqldbrestdataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, fpjson, fpjsondataset;

Type

  { TSQLDBRestDataset }

  TSQLDBRestDataset = Class(TBaseJSONDataset)
  private
  Protected
    function GetStringFieldLength(F: TJSONObject; AName: String; AIndex: Integer): integer;virtual;
    function StringToFieldType(S: String): TFieldType; virtual;
    Function CreateFieldMapper: TJSONFieldMapper; override;
    Procedure MetaDataToFieldDefs; override;
  Public
    procedure LoadFromStream(S: TStream);
  end;

implementation

type
  PRecInfo = ^TRecInfo;
  TRecInfo = record
    Index: Integer;
    Bookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;


procedure TSQLDBRestDataset.LoadFromStream(S: TStream);

Var
  D : TJSONData;
  O : TJSONObject absolute D;
  N : String;
  I : Integer;

begin
  D:=GetJSON(S);
  try
    if (D.JSONType<>jtObject) then
      Raise EJSONDataset.Create('Not a valid JSON data packet');
    N:='data';
    // Check metadata
    I:=O.IndexOfName('metaData');
    if (I<>-1) then
      begin
      If (O.Items[i].JSONType<>jtObject) then
        Raise EJSONDataset.Create('Invalid JSON metaData in data packet.');
      Metadata:=O.Objects['metaData'];
      O.Extract(I);
      end;
    // Check rows
    I:=O.IndexOfName(N);
    if (I=-1) then
      Raise EJSONDataset.Create('Missing data in data packet');
    if (O.Items[i].JSONType<>jtArray) then
      Raise EJSONDataset.Create('Rows element must be an array');
    Rows:=O.Items[i] as TJSONArray;
    O.Extract(I);
    OwnsData:=True;
  finally
    D.Free;
  end;
end;

function TSQLDBRestDataset.StringToFieldType(S: String): TFieldType;

begin
  if (s='int') then
    Result:=ftInteger
  else if (s='bigint') then
      Result:=ftLargeInt
  else if (s='float') then
    Result:=ftFloat
  else if (s='bool') then
    Result:=ftString // Buggy TJSONDataset in 3.0.4
  else if (s='date') then
    Result:=ftDate
  else if (s='datetime') then
    Result:=ftDateTime
  else if (s='time') then
    Result:=ftTime
  else if (s='blob') then
    Result:=ftBlob
  else if (s='string') then
    Result:=ftString
  else
    if MapUnknownToStringType then
      Result:=ftString
    else
      Raise EJSONDataset.CreateFmt('Unknown JSON data type : %s',[s]);
end;

function TSQLDBRestDataset.CreateFieldMapper: TJSONFieldMapper;
begin
  Result:=TJSONObjectFieldMapper.Create;
end;


function TSQLDBRestDataset.GetStringFieldLength(F: TJSONObject; AName: String;
  AIndex: Integer): integer;

Var
  I,L : Integer;
  D : TJSONData;

begin
  Result:=F.Get('maxLen',0);
  if (Result=0) then
    Result:=255;
end;

procedure TSQLDBRestDataset.MetaDataToFieldDefs;
Var
  A : TJSONArray;
  F : TJSONObject;
  I,FS : Integer;
  N,D: String;
  ft: TFieldType;

begin
  FieldDefs.Clear;
  A:=Metadata.Get('fields',TJSONArray(Nil));
  if A=Nil then
    Raise EJSONDataset.Create('Invalid metadata object');
  For I:=0 to A.Count-1 do
    begin
    F:=A.Objects[i];
    N:=F.Get('name','');
    If N='' then
      Raise EJSONDataset.CreateFmt('Field definition %d in has no or invalid name property',[i]);
    D:=F.Get('type','');
    If (D='') then
      ft:=ftstring
    else
     ft:=StringToFieldType(String(D));
    if (ft=ftString) then
      fs:=GetStringFieldLength(F,N,I)
    else
      fs:=0;
    FieldDefs.Add(N,ft,fs);
    end;
end;

end.

