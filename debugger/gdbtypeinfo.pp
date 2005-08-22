{ $Id$ }
{                        ----------------------------------------------
                            GDBTypeInfo.pp  -  Debugger helper class 
                         ----------------------------------------------

 @created(Wed Mar 29th WET 2003)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains a helper class for decoding PType output.


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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit GDBTypeInfo;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils;

(*
  ptype = {
     family = "class" | "record" | "enum" | "set" | "procedure" | "function" | "simple" | "pointer"
    [ ancestor = "...", ]
    [ private = "[" ( "{" name = "...", type = ptype "}" )* "}," ]
    [ protected = "[" ( "{" name = "...", type = ptype "}" )* "}," ]
    [ public = "[" ( "{" name = "...", type = ptype "}" )* "},"]
    [ published = "[" ( "{" name = "...", type = ptype "}" )* "}," ]
    [ members = "[" ( "..." )* "]," | "[" ( "{" name = "...", type = "..." "}" )* "]," ]
    [ args = "[" ( "..." )* "]," ]
    [ result = "..." ]
    [ name = "..." ]
    [ type = "..." ]
*)
type
  TGDBSymbolKind = (skClass, skRecord, skEnum, skSet, skProcedure, skFunction, skSimple, skPointer);
  TGDBFieldLocation = (flPrivate, flProtected, flPublic, flPublished);
  TGDBFieldFlag = (ffVirtual);
  TGDBFieldFlags = set of TGDBFieldFlag;

  TGDBType = class;
  TGDBField = class(TObject)
  private
    FName: String;
    FFlags: TGDBFieldFlags;
    FLocation: TGDBFieldLocation;
    FGDBType: TGDBType;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Name: String read FName;
    property GDBType: TGDBType read FGDBType;
    property Location: TGDBFieldLocation read FLocation;
    property Flags: TGDBFieldFlags read FFlags;
  end;

  TGDBFields = class(TObject)
  private
    FList: TList;
    function GetField(const AIndex: Integer): TGDBField;
    function GetCount: Integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TGDBField read GetField; default;
  end;

  TGDBTypes = class(TObject)
  private
    FList: TList;
    function GetType(const AIndex: Integer): TGDBType;
    function GetCount: Integer;
  protected
  public
    constructor Create;
    constructor CreateFromCSV(AValues: String);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TGDBType read GetType; default;
  end;

  { TGDBType }

  TGDBType = class(TObject)
  private
    FAncestor: String;
    FResult: TGDBType;
    FArguments: TGDBTypes;
    FFields: TGDBFields;
    FKind: TGDBSymbolKind;
    FMembers: TStrings;
    FTypeName: String;
  protected
  public
    constructor Create;
    constructor CreateFromValues(const AValues: String);
    destructor Destroy; override;
    property Ancestor: String read FAncestor;
    property Arguments: TGDBTypes read FArguments;
    property Fields: TGDBFields read FFields;
    property Kind: TGDBSymbolKind read FKind;
    property TypeName: String read FTypeName;
    property Members: TStrings read FMembers;
    property Result: TGDBType read FResult;
  end;


function CreatePTypeValueList(AResultValues: String): TStringList;

implementation

function GetPart(const ASkipTo, AnEnd: array of String; var ASource: String): String;
var
  n, i, idx, SkipLen: Integer;
begin
  idx := 0;
  SkipLen := 0;
  if High(ASkipTo) <> -1
  then begin
    for n := Low(ASkipTo) to High(ASkipTo) do
    begin
      if ASkipTo[n] <> ''
      then begin
        i := Pos(ASkipTo[n], ASource);
        if (i > 0) and ((idx = 0) or (i < idx))
        then begin
          idx := i;
          SkipLen := Length(ASkipTo[n]);
        end;
      end;
    end;
    if idx = 0
    then begin
      Result := '';
      Exit;
    end;
    Delete(ASource, 1, idx + SkipLen - 1);
  end;

  idx := 0;
  for n := Low(AnEnd) to High(AnEnd) do
  begin
    if AnEnd[n] <> ''
    then begin
      i := Pos(AnEnd[n], ASource);
      if (i > 0) and ((idx = 0) or (i < idx))
      then idx := i;
    end;
  end;

  if idx = 0
  then begin
    Result := ASource;
    ASource := '';
  end
  else begin
    Result := Copy(ASource, 1, idx - 1);
    Delete(ASource, 1, idx - 1);
  end;
end;

function CreatePTypeValueList(AResultValues: String): TStringList;
var
  S, Line: String;
  Lines: TStringList;

  procedure DoRecord;
  var
    n: Integer;
    S, Members: String;
  begin
    Result.Add('family=record');
    Members := '';

    //concatinate all lines and skip last end
    S := '';
    for n := 0 to Lines.Count - 2 do
      S := S + Lines[n];

    while S <> '' do
    begin
      if Members <> '' then Members := Members + ',';
      Members := Members + '{name=' + GetPart(['    '], [' '], S);
      Members := Members + ',type=' + GetPart([' : '], [';'], S) + '}';
    end;
    Result.Add('members=[' + Members + ']');
  end;

  procedure DoEnum;
  var
    n: Integer;
    S: String;
  begin
    Result.Add('family=enum');

    S := GetPart(['('], [], Line);
    //concatinate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    S := GetPart([], [')'], S);
    Result.Add('members=[' + StringReplace(S, ' ', '', [rfReplaceAll]) + ']');
  end;

  procedure DoProcedure;
  var
    n: Integer;
    S: String;
  begin
    Result.Add('family=procedure');

    S := GetPart(['('], [''], Line);
    //concatinate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    S := GetPart([''], [')'], S);
    Result.Add('args=[' + StringReplace(S, ', ', ',', [rfReplaceAll]) + ']');
  end;

  procedure DoFunction;
  var
    n: Integer;
    S, Args: String;
  begin
    Result.Add('family=function');

    S := GetPart(['('], [], Line);
    //concatinate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    Args := GetPart([], [')'], S);
    S := GetPart([' : '], [], S);
    Result.Add('args=[' + StringReplace(Args, ', ', ',', [rfReplaceAll]) + ']');
    Result.Add('result=' + S);
  end;

  procedure DoClass;
  begin
    Result.Add('family=class');
    Result.Add('ancestor=' + GetPart([': public '], [' '], Line));
  end;

begin
  Result := TStringList.Create;
  if AResultValues = '' then Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := AResultValues;
    if Lines.Count = 0 then Exit;
    Line := Lines[0];
    Lines.Delete(0);

    S := GetPart(['type = '], [' '], Line);
    if S = '' then Exit;
    if Pos(' = class ', Line) > 0
    then DoClass
    else if S[1] = '^'
    then begin
      Result.Add('family=pointer');
      Result.Add('type=' + GetPart(['^'], [' ='], S));
    end
    else if S = 'set'
    then begin
      Result.Add('family=set');
      Result.Add('type=' + Copy(Line, 5, Length(Line)));
    end
    else if S = 'procedure'
    then DoProcedure
    else if S = 'function'
    then DoFunction
    else if Pos(' = (', Line) > 0
    then DoEnum
    else if Pos(' = record', Line) > 0
    then DoRecord
    else begin
      Result.Add('family=simple');
      Result.Add('type=' + S);
    end;

  finally
    Lines.Free;
  end;
end;

{ TGDBField }

constructor TGDBField.Create;
begin
  FFlags := [];
  FGDBType := nil;
  FLocation := flPublic;
end;

destructor TGDBField.Destroy;
begin
  if FGDBType<>nil then FreeAndNil(FGDBType);
  inherited Destroy;
end;

{ TGDBFields }

constructor TGDBFields.Create;
begin
  FList := TList.Create;
  inherited;
end;

destructor TGDBFields.Destroy;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    Items[n].Free;

  FreeAndNil(FList);
  inherited;
end;

function TGDBFields.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGDBFields.GetField(const AIndex: Integer): TGDBField;
begin
  Result := TGDBField(FList[AIndex]);
end;

{ TGDBPType }

constructor TGDBType.Create;
begin
  FResult := nil;
  FArguments := nil;
  FFields := nil;
  FMembers := nil;

  inherited Create;
end;

constructor TGDBType.CreateFromValues(const AValues: String);
var
  S, Line: String;
  Lines: TStringList;

  procedure DoRecord;
  var
    n: Integer;
    S: String;
    Field: TGDBField;
  begin
    FKind := skRecord;
    FFields := TGDBFields.Create;

    //concatenate all lines and skip last end
    S := '';
    for n := 0 to Lines.Count - 2 do
      S := S + Lines[n];

    while S <> '' do
    begin
      Field := TGDBField.Create;
      Field.FName := GetPart(['    '], [' '], S);
      Field.FLocation := flPublic;
      Field.FGDBType := TGDBType.Create;
      Field.FGDBType.FKind := skSimple; // for now
      Field.FGDBType.FTypeName := GetPart([' : '], [';'], S);
      FFields.FList.Add(Field);
      Delete(S, 1, 1);
    end;
  end;

  procedure DoEnum;
  var
    n: Integer;
    S: String;
  begin
    FKind := skEnum;

    S := GetPart(['('], [], Line);
    //concatenate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    S := GetPart([], [')'], S);
    FMembers := TStringList.Create;
    FMembers.Text := StringReplace(S, ' ', #13#10, [rfReplaceAll]);
  end;

  procedure DoSet;
  var
    n: Integer;
    S: String;
  begin
    FKind := skSet;

    S := Copy(Line, 5, Length(Line));
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    if Pos('=', S) = 0
    then FTypeName := S
    else begin
      S := GetPart(['('], [')'], S);
      FMembers := TStringList.Create;
      FMembers.Text := StringReplace(StringReplace(S, ',', #13#10, [rfReplaceAll]), ' ', '', [rfReplaceAll]);
    end;
  end;

  procedure DoProcedure;
  var
    n: Integer;
    S: String;
  begin
    FKind := skProcedure;

    S := GetPart(['('], [], Line);
    //concatenate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    S := GetPart([], [')'], S);
    FArguments := TGDBTypes.CreateFromCSV(S);
  end;

  procedure DoFunction;
  var
    n: Integer;
    S: String;
  begin
    FKind := skFunction;

    S := GetPart(['('], [], Line);
    //concatenate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    FArguments := TGDBTypes.CreateFromCSV(GetPart([], [')'], S));
    FResult := TGDBType.Create;
    FResult.FKind := skSimple; // for now
    FResult.FTypeName := GetPart([' : '], [], S);
  end;

  procedure DoClass;
  var
    n: Integer;
    Field: TGDBField;
    S: String;
    Location: TGDBFieldLocation;
  begin
    FKind := skClass;
    FAncestor := GetPart([': public '], [' '], Line);
    FFields := TGDBFields.Create;

    Location := flPublished;
    for n := 0 to Lines.Count - 2 do
    begin
      S := Lines[n];
      if S = '' then Continue;
      if S = '  private' then Location := flPrivate
      else if S = '  protected' then Location := flProtected
      else if S = '  public' then Location := flPublic
      else if S = '  published' then Location := flPublished
      else begin
        Field := TGDBField.Create;
        Field.FLocation := Location;
        Field.FGDBType := TGDBType.Create;
        FFields.FList.Add(Field);

        if Pos(' procedure ', S) > 0
        then begin
          Field.FName := GetPart(['procedure '], [' ', ';'], S);
          Field.FGDBType.FKind := skProcedure;
          Field.FGDBType.FArguments := TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S));
          if GetPart(['; '], [';'], S) = 'virtual'
          then Field.FFlags := [ffVirtual];
        end
        else if Pos(' function ', S) > 0
        then begin
          Field.FName := GetPart(['function  '], [' ', ';'], S);
          Field.FGDBType.FKind := skFunction;
          Field.FGDBType.FArguments := TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S));
          Field.FGDBType.FResult := TGDBType.Create;
          Field.FGDBType.FResult.FKind := skSimple; // for now
          Field.FGDBType.FResult.FTypeName := GetPart([' : '], [';'], S);
          if GetPart(['; '], [';'], S) = 'virtual'
          then Field.FFlags := [ffVirtual];
        end
        else begin
          Field.FName := GetPart(['    '], [' '], S);
          Field.FGDBType.FKind := skSimple; // for now
          Field.FGDBType.FTypeName := GetPart([' : '], [';'], S);
        end;
      end;
    end;
  end;
begin
  Create;

  if AValues = '' then Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := AValues;
    if Lines.Count = 0 then Exit;

    Line := Lines[0];
    Lines.Delete(0);

    S := GetPart(['type = '], [' '], Line);
    if S = '' then Exit;
    if Pos(' = class ', Line) > 0
    then begin
      FTypeName := GetPart(['^'], [' '], S);
      DoClass;
    end
    else if S[1] = '^'
    then begin
      FKind := skPointer;
      FTypeName := GetPart(['^'], [' ='], S);
    end
    else if S = 'set'
    then DoSet
    else if S = 'procedure'
    then DoProcedure
    else if S = 'function'
    then DoFunction
    else if Pos(' = (', Line) > 0
    then DoEnum
    else if Pos(' = record', Line) > 0
    then DoRecord
    else begin
      FKind := skSimple;
      FTypeName := S;
    end;

  finally
    Lines.Free;
  end;
end;

destructor TGDBType.Destroy;
begin
  if FResult<>nil then FreeAndNil(FResult);
  if FArguments<>nil then FreeAndNil(FArguments);
  if FFields<>nil then FreeAndNil(FFields);
  if FMembers<>nil then FreeAndNil(FMembers);

  inherited;
end;

{ TGDBPTypes }

constructor TGDBTypes.Create;
begin
  FList := TList.Create;
  inherited;
end;

constructor TGDBTypes.CreateFromCSV(AValues: String);
var
  GDBType: TGDBType;
begin
  Create;
  while AValues <> '' do
  begin
    GDBType := TGDBType.Create;
    GDBType.FKind := skSimple;
    GDBType.FTypeName := GetPart([], [', '], AValues);
    FList.Add(GDBType);
    {if Length(AValues) >= 2 then} Delete(AValues, 1, 2);
  end;
end;

destructor TGDBTypes.Destroy;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    Items[n].Free;
    
  FreeAndNil(FList);
  inherited;
end;

function TGDBTypes.GetCount: Integer;
begin
  Result := Flist.Count;
end;

function TGDBTypes.GetType(const AIndex: Integer): TGDBType;
begin
  Result := TGDBType(FList[AIndex]);
end;

end.
