{
girpascalwriter.pas
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
unit girpascalwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,girNameSpaces, girpascalwritertypes;

type


  { TgirPascalWriter }

  TgirPascalWriter = class
  private
    FDefaultUnitExtension: String;
    FUnitPrefix: String;
    FOnUnitWriteEvent: TgirWriteEvent;
    FNameSpaces: TgirNamespaces;
    FUnits: TList;
    FOptions: TgirOptions;
  public
     constructor Create(ANameSpaces: TgirNamespaces; AOptions: TgirOptions; AUnitPrefix: String);
     procedure GenerateUnits;
     property OnUnitWriteEvent: TgirWriteEvent read FOnUnitWriteEvent write FOnUnitWriteEvent;
     property DefaultUnitExtension: String read FDefaultUnitExtension write FDefaultUnitExtension; // is .pas by default
     property Units: TList read FUnits;
  end;


implementation
uses girCTypesMapping;


{ TgirPascalWriter }

constructor TgirPascalWriter.Create(ANameSpaces: TgirNamespaces; AOptions: TgirOptions; AUnitPrefix: String);
begin
  FNameSpaces := ANameSpaces;
  FUnitPrefix := AUnitPrefix;
  FUnits := TList.Create;
  FDefaultUnitExtension:='.pas';
  FOptions:=AOptions;
  FUnitPrefix:=AUnitPrefix;
end;

procedure TgirPascalWriter.GenerateUnits;
var
  i: Integer;
  UnitGroup: TPascalUnitGroup;


begin
  for i := 0 to FNameSpaces.Count-1 do
    begin
      WriteLn(Format('Converting %s', [FNameSpaces.NameSpace[i].NameSpace]));
      UnitGroup := TPascalUnitGroup.Create(Self, FNameSpaces.NameSpace[i], FOptions, FUnitPrefix);
      UnitGroup.GenerateUnits;
    end;
end;

end.

