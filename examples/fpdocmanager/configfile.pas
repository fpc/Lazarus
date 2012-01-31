unit ConfigFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TConfigFile }

  TConfigFile = class
  private
    FDirty: Boolean;
    FFileName: string;
    FSections: TStringList;
    procedure LoadFromFile;
    function  AddSection(s: string): TStringList;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function  ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    function  ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure WriteSection(const Section: string; Strings: TStrings);
    procedure WriteSectionValues(const Section: string; Strings: TStrings);
    function  FindSection(Section: string): TStringList;
    function  SectionExists(const Section: string): Boolean;
    procedure Flush;
    property Dirty : Boolean Read FDirty;
    property FileName: string read FFileName;
    property Sections: TStringList read FSections;
  end;

implementation

{ TConfigFile }

constructor TConfigFile.Create(const AFileName: string);
begin
  FSections := TStringList.Create;
  FSections.OwnsObjects := True;
  if AFileName = '' then
    exit; //nothing to do
  FFileName:=AFileName;
  LoadFromFile;
end;

destructor TConfigFile.Destroy;
begin
  Flush;
  FreeAndNil(FSections); //recursive!
  inherited Destroy;
end;

procedure TConfigFile.LoadFromFile;
var
  lst, sec: TStringList;
  s: string;
  i: integer;
begin
  if (FFileName = '') or not FileExists(FFileName) then
    exit; //nothing to load
  lst := TStringList.Create;
  lst.LoadFromFile(FFileName);
  sec := nil;
  for i := 0 to lst.Count - 1 do begin
    s := lst[i];
    if s = '' then
      continue;
    if s[1] = '[' then
      sec := AddSection(s)
    else
      sec.Add(s);
  end;
  lst.Free;
  FDirty:=False; //in case it was set during initialization
end;

function TConfigFile.FindSection(Section: string): TStringList;
var
  i: integer;
begin
  Result := nil;
  if Section = '' then
    exit;
  if Section[1] = '[' then
    Section := Copy(Section, 2, Length(Section)-2); //strip []
  i := FSections.IndexOf(Section);
  if i >= 0 then
    TObject(Result) := FSections.Objects[i];
end;

function TConfigFile.SectionExists(const Section: string): Boolean;
begin
  Result := FindSection(Section) <> nil;
end;

function TConfigFile.AddSection(s: string): TStringList;
var
  i: integer;
begin
  Result := nil;
  if s = '' then
    exit;
  if s[1] = '[' then
    s := Copy(s, 2, Length(s)-2);
  i := FSections.IndexOf(s);
  if i < 0 then begin
    Result := TStringList.Create;
    FSections.AddObject(s, Result);
    FDirty:=True;
  end else
    TObject(Result) := FSections.Objects[i];
end;

function TConfigFile.ReadString(const Section, Ident, Default: string): string;
var
  sec: TStringList;
  i: integer;
begin
  sec := FindSection(Section);
  if sec = nil then
    exit(Default);
  i := sec.IndexOfName(Ident);
  if i < 0 then
    Result := Default
  else
    Result := sec.ValueFromIndex[i];
end;

procedure TConfigFile.WriteString(const Section, Ident, Value: String);
var
  sec: TStringList;
  i: integer;
  s: string;
begin
  if (Ident = '') {or (Value = '')} then
    exit; //invalid Ident
  sec := AddSection(Section);
  s := Ident + '=' + Value;
  i := sec.IndexOf(s);
  if i >= 0 then
    exit; //already stored
  i := sec.IndexOfName(ident);
  if i < 0 then
    sec.Add(s)
  else
    sec.Strings[i] := s;
  FDirty:=True;
end;

function TConfigFile.ReadBool(const Section, Ident: string; Default: Boolean
  ): Boolean;
var
  s: string;
begin
  s := ReadString(Section, Ident, '');
  if s = '' then
    Result := Default
  else
    Result := s <> '0';
end;

procedure TConfigFile.WriteBool(const Section, Ident: string; Value: Boolean);
const
  aValues: array[boolean] of string = ('0', '1');
begin
  WriteString(Section, Ident, aValues[Value]);
end;

procedure TConfigFile.ReadSection(const Section: string; Strings: TStrings);
var
  sec: TStringList;
begin
  Strings.Clear;
  sec := FindSection(Section);
  if sec <> nil then
    Strings.Assign(sec);
end;

procedure TConfigFile.WriteSection(const Section: string; Strings: TStrings);
var
  sec: TStringList;
begin
  sec := AddSection(Section);
  if not sec.Equals(Strings) then begin
    sec.Assign(Strings);
    FDirty:=True;
  end;
end;

procedure TConfigFile.WriteSectionValues(const Section: string;
  Strings: TStrings);
begin
  WriteSection(Section, Strings);
end;

procedure TConfigFile.Flush;
var
  lst, sec: TStringList;
  i: integer;
  s: string;
begin
  if (not FDirty) or (FFileName = '') then
    exit;
  lst := TStringList.Create;
  for i := 0 to FSections.Count - 1 do begin
    s := FSections[i];
    TObject(sec) := FSections.Objects[i];
    lst.Add('[' + s + ']');
    lst.AddStrings(sec);
  end;
  lst.SaveToFile(FFileName);
  lst.Free;
  FDirty:=False;
end;

end.

