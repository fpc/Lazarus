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
  Classes, SysUtils, Debugger, LclProc;

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
  { TGDBTypes }

  TGDBTypes = class(TDBGTypes)
  public
    constructor CreateFromCSV(AValues: String);
  end;

  { TGDBType }

  TGDBType = class(TDBGType)
  private
    FInternalTypeName: string;
  public
    constructor CreateFromValues(const AValues: String;
                                 AWhatIsValue: String = '';
                                 const AWhatIsType: String = '';
                                 AClassIsPointer: Boolean = False);
    // InternalTypeName: include ^ for TObject, if needed
    property InternalTypeName: string read FInternalTypeName;
  end;


function CreatePTypeValueList(AResultValues: String): TStringList;
function ParseTypeFromGdb(const ATypeText: string): string;

implementation

(*
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
*)

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

function ParseTypeFromGdb(const ATypeText: string): string;
var
  StartIdx, EndIdx, BracketCnt, ln: Integer;
  EndPtr: PChar;
begin
  Result := '';
  StartIdx := pos('type = ', ATypeText);
  if StartIdx <= 0 then exit;
  inc(StartIdx, 7);
  EndIdx := StartIdx;
  EndPtr := @ATypeText[EndIdx];
  ln := length(ATypeText);
  BracketCnt := 0;
  while (EndIdx <= ln) do begin
    case EndPtr^ of
      ' ' : if BracketCnt = 0 then break;
      '[', '{' : inc(BracketCnt);
      ']', '}' : dec(BracketCnt);
      '\' : if (EndIdx < ln) and ((EndPtr+1)^ = 'n') then break;
      #0..#31: break;
    end;
    inc(EndPtr);
    inc(EndIdx);
  end;
  Result := copy(ATypeText, StartIdx, EndIdx-StartIdx);
end;

{ TGDBPType }

constructor TGDBType.CreateFromValues(const AValues: String;
  AWhatIsValue: String = ''; const AWhatIsType: String = '';
  AClassIsPointer: Boolean = False);
var
  S, Line: String;
  Lines: TStringList;

  procedure DoRecord;
  var
    n: Integer;
    S: String;
    Field: TDBGField;
  begin
    if (FTypeName = 'Variant') or
       (FTypeName = 'VARIANT') then
      FKind := skVariant
    else
    if (FTypeName = 'ShortString') or
       (FTypeName = 'SHORTSTRING') or
       (FTypeName = '&ShortString') then
      FKind := skSimple
    else
      FKind := skRecord;

    FFields := TDBGFields.Create;

    //concatenate all lines and skip last end
    S := '';
    for n := 0 to Lines.Count - 2 do
      S := S + Lines[n];

    while S <> '' do
    begin
      Field := TDBGField.Create(
        GetPart(['    '], [' '], S),
        TGDBType.Create(skSimple, GetPart([' : '], [';'], S)),
        flPublic
      );
      FFields.Add(Field);
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
    FResult := TGDBType.Create(skSimple, GetPart([' : '], [], S));
  end;

  procedure DoClass;
  var
    n: Integer;
    S: String;

    Name: String;
    DBGType: TDBGType;
    Location: TDBGFieldLocation;
    Flags: TDBGFieldFlags;
  begin
    FKind := skClass;
    FAncestor := GetPart([': public '], [' '], Line);
    FFields := TDBGFields.Create;

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
        Flags := [];
        if Pos(' procedure ', S) > 0
        then begin
          Name := GetPart(['procedure '], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skProcedure,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
        end
        else if Pos(' destructor  ~', S) > 0
        then begin
          Name := GetPart(['destructor  ~'], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skProcedure,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
          Include(Flags, ffDestructor);
        end
        else if Pos(' constructor ', S) > 0
        then begin
          Name := GetPart(['constructor '], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skFunction,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S)),
            TGDBType.Create(skSimple, GetPart([' : '], [';'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
          Include(Flags, ffConstructor);
        end
        else if Pos(' function ', S) > 0
        then begin
          Name := GetPart(['function  '], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skFunction,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S)),
            TGDBType.Create(skSimple, GetPart([' : '], [';'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
        end
        else begin
          Name := GetPart(['    '], [' '], S);
          DBGType := TGDBType.Create(skSimple, GetPart([' : '], [';'], S));
        end;

        FFields.Add(TDBGField.Create(Name, DBGType, Location, Flags));
      end;
    end;
  end;

var
  ParsedWhatIsValue, ParsedWhatIsType: string;
begin
  ParsedWhatIsValue := ParseTypeFromGdb(AWhatIsValue);
  ParsedWhatIsType := ParseTypeFromGdb(AWhatIsType);

  Lines := TStringList.Create;
  try
    Lines.Text := AValues;

    if Lines.Count > 0 then begin
      Line := Lines[0];
      Lines.Delete(0);
    end
    else Line := '';
    S := ParseTypeFromGdb(Line);

    FAttributes := [];
    if (ParsedWhatIsValue <> '') and (ParsedWhatIsValue[1] = '&') then begin
      Delete(ParsedWhatIsValue, 1, 1);
      include(FAttributes, saRefParam);
    end;

    Create(skSimple, ParsedWhatIsValue);
    FInternalTypeName := ParsedWhatIsValue;

    if Pos(' = class ', Line) > 0 then begin
      // Class or pointer to class
      if AClassIsPointer and (S[1] = '^') then begin
        // class: dwarf type, always pefixed with ^

        if (length(s) >= 2) and (s[2] = '^')
        then begin
          FKind:=skPointer;
          if FTypeName = ''
          then FTypeName := copy(s, 3, length(s));
        end
        else if (ParsedWhatIsValue <> '') and (ParsedWhatIsValue[1] = '^')
        and (pos(' = class', AWhatIsValue) <= 0)
        then begin
          FKind:=skPointer; // pointer to another named type
          if FTypeName = ''
          then FTypeName := copy(ParsedWhatIsValue, 2, length(ParsedWhatIsValue));
        end
        else if (ParsedWhatIsType <> '') and (ParsedWhatIsType[1] = '^')
        and (pos(' = class', AWhatIsType) <= 0)
        then begin
          FKind:=skPointer; // pointer to another named type
          if FTypeName = ''
          then FTypeName := copy(ParsedWhatIsType, 2, length(ParsedWhatIsType));
        end
        else begin
          include(FAttributes, saInternalPointer);
          if FTypeName = ''
          then FTypeName := GetPart([], ['{'], S);
          DoClass;
        end;

        if FInternalTypeName = ''
        then FInternalTypeName := FTypeName;

        if (FInternalTypeName <> '') and (FInternalTypeName[1] <> '^')
        and (saInternalPointer in FAttributes)
        then FInternalTypeName := '^' + FInternalTypeName;

      end
      else begin
        // class: stabs type, not normaly prefixed

        if (length(s) >= 1) and (s[1] = '^')
        then begin
          FKind:=skPointer;
          if FTypeName = ''
          then FTypeName := copy(s, 2, length(s));
        end
        else begin
          include(FAttributes, saInternalPointer);
          if FTypeName = ''
          then FTypeName := GetPart([], ['{'], S);
          DoClass;
        end;

        if FInternalTypeName = ''
        then FInternalTypeName := FTypeName;
      end;
    end

    else
    if (S[1] = '^')
    or ((ParsedWhatIsValue <> '') and (ParsedWhatIsValue[1] = '^'))
    or ((ParsedWhatIsType <> '') and (ParsedWhatIsType[1] = '^'))
    then begin
      FKind := skPointer;
      if FTypeName = ''
      then FTypeName := GetPart(['^'], [' ='], S);
      // strip brackets
      FTypeName := GetPart(['(', ''], [')'], FTypeName);
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
    then begin
      if FTypeName = ''
      then FTypeName := S;
      DoRecord
    end
    else if S = 'record'
    then begin
      // unnamed record (classtype ??)
      DoRecord;
    end
    else begin
      FKind := skSimple;
      if FTypeName = ''
      then FTypeName := S;
    end;

  finally
    Lines.Free;
  end;
end;

{ TGDBPTypes }

constructor TGDBTypes.CreateFromCSV(AValues: String);
var
  GDBType: TGDBType;
begin
  Create;
  while AValues <> '' do
  begin
    GDBType := TGDBType.Create(skSimple, GetPart([], [', '], AValues));
    FList.Add(GDBType);
    {if Length(AValues) >= 2 then} Delete(AValues, 1, 2);
  end;
end;

end.
