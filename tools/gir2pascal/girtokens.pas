{
girtokens.pas
Copyright (C) 2011  Andrew Haines andrewd207@aol.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
unit girTokens;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TGirToken = (gtInvalid, gtEmpty, gtAlias, gtConstant, gtRecord, gtBitField, gtEnumeration,
               gtCallback, gtUnion, gtFunction, gtReturnValue, gtType,
               gtParameters, gtParameter, gtInstanceParameter, gtMember, gtField, gtMethod, gtArray,
               gtDoc, gtDocDeprecated, gtConstructor, gtRepository, gtInclude, gtNameSpace, gtPackage,
               gtCInclude, gtClass, gtProperty, gtVirtualMethod, gtInterface,
               gtGlibSignal, gtImplements, gtPrerequisite,gtVarArgs, gtObject, gtClassStruct, gtGType,
               // Direction for parameters.  in is default = no pointer. out and inout means one pointer level.
               // If subnode is array then increase pointer level.
               gtIn, gtOut, gtInOut, gtSourcePosition
               );

  TGirVersion = object
    Major: Integer;
    Minor: Integer;
    function AsString: String; // '$major.$minor'
    function AsMajor: TGirVersion; // return as $major.0 i.e 3.0 instead of 3.8

  end;


var
  GirTokenName: array[TGirToken] of String = (
    'Invalid Name',
    '{empty}',
    'alias',
    'constant',
    'record',
    'bitfield',
    'enumeration',
    'callback',
    'union',
    'function',
    'return-value',
    'type',
    'parameters',
    'parameter',
    'instance-parameter',
    'member',
    'field',
    'method',
    'array',
    'doc',
    'doc-deprecated',
    'constructor',
    'repository',
    'include',
    'namespace',
    'package',
    'c:include',
    'class',
    'property',
    'virtual-method',
    'interface',
    'glib:signal',
    'implements',
    'prerequisite',
    'varargs',
    'object',
    'classstruct',
    'gtype',
    'in',
    'out',
    'inout',
    'source-position'
  );

  function GirTokenNameToToken(AName: String): TGirToken;
  function girVersion(AVersion: String; ADefaultMajor: Integer = -1; ADefaultMinor: Integer = -1): TGirVersion;
  function girVersion(AMajor, AMinor: Integer): TGirVersion;

  operator >= (AVersion, BVersion: TGirVersion): Boolean;
  operator <= (AVersion, BVersion: TGirVersion): Boolean;
  operator > (AVersion, BVersion: TGirVersion): Boolean;

implementation
uses
  sysutils;

function GirTokenNameToToken(AName: String): TGirToken;
begin
  if AName = '' then
    Exit(gtEmpty);
  try
  for Result in TGirToken do
    if GirTokenName[Result][1] <> AName[1] then
      continue
    else if GirTokenName[Result] = AName then
      Exit;
  Result := gtInvalid;

  except
    WriteLn('GirToken Exception: ',AName);
  end;
end;

function girVersion(AVersion: String; ADefaultMajor: Integer; ADefaultMinor: Integer): TGirVersion;
var
  SplitPoint: Integer;
  Minor: String;
begin
  if (AVersion = '') and (ADefaultMajor <> -1) and (ADefaultMinor <> -1) then
  begin
    Result := girVersion(ADefaultMajor, ADefaultMinor);
    Exit;
  end;
  SplitPoint := Pos('.', AVersion);

  if SplitPoint < 1 then
    raise Exception.Create(Format('Invalid version string format: "%s" (length %d)', [AVersion, Length(AVersion)]));

  Result.Major:=StrToInt(Copy(AVersion,1, SplitPoint-1));
  Minor := Copy(AVersion,SplitPoint+1, MaxInt);
  SplitPoint := Pos('.', Minor);
  // we are not interested in the third version chunk
  if SplitPoint > 0 then
    Minor := Copy(Minor,1, SplitPoint-1);
  Result.Minor:=StrToInt(Minor);
end;

function girVersion(AMajor, AMinor: Integer): TGirVersion;
begin
  REsult.Major := AMajor;
  Result.Minor := AMinor;
end;

operator >= (AVersion, BVersion: TGirVersion): Boolean;
begin
  Result :=     (AVersion.Major > BVersion.Major)
            or ((AVersion.Major = BVersion.Major) and (AVersion.Minor >= BVersion.Minor));
end;

operator<=(AVersion, BVersion: TGirVersion): Boolean;
begin
  Result :=     (AVersion.Major < BVersion.Major)
            or ((AVersion.Major = BVersion.Major) and (AVersion.Minor <= BVersion.Minor));
end;

operator > (AVersion, BVersion: TGirVersion): Boolean;
begin
  Result :=     (AVersion.Major > BVersion.Major)
            or ((AVersion.Major = BVersion.Major) and (AVersion.Minor > BVersion.Minor));
end;

{ TGirVersion }

function TGirVersion.AsString: String;
begin
  Result := IntToStr(Major)+'.'+IntToStr(Minor);
end;

function TGirVersion.AsMajor: TGirVersion;
begin
  Result.Major:=Major;
  REsult.Minor:=0;
end;

end.

