{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   Useful functions for IDE add ons.
}
unit IDEUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, LazUTF8, LazFileUtils;

type
  TCmpStrType = (
    cstCaseSensitive,
    cstCaseInsensitive,
    cstFilename
    );

function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
procedure SetComboBoxText(AComboBox: TComboBox; const AText: String;
                          Cmp: TCmpStrType; MaxCount: integer = 1000);
function LazIsValidIdent(const Ident: string; AllowDots: Boolean = False;
                         StrictDots: Boolean = False): Boolean;

implementation

function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do begin
    case Cmp of
    cstCaseSensitive: if List[i]=s then exit(i);
    cstCaseInsensitive: if UTF8CompareText(List[i],s)=0 then exit(i);
    cstFilename: if CompareFilenames(List[i],s)=0 then exit(i);
    end;
  end;
  Result:=-1;
end;

procedure SetComboBoxText(AComboBox:TComboBox; const AText: String;
  Cmp: TCmpStrType; MaxCount: integer);
var
  a: integer;
begin
  a := IndexInStringList(AComboBox.Items,Cmp,AText);
  if a >= 0 then
    AComboBox.ItemIndex := a
  else
  begin
    AComboBox.Items.Insert(0,AText);
    AComboBox.ItemIndex:=IndexInStringList(AComboBox.Items,Cmp,AText);
    if MaxCount<2 then MaxCount:=2;
    while AComboBox.Items.Count>MaxCount do
      AComboBox.Items.Delete(AComboBox.Items.Count-1);
  end;
  AComboBox.Text := AText;
end;

function LazIsValidIdent(const Ident: string; AllowDots: Boolean = False;
                         StrictDots: Boolean = False): Boolean;
// This is a copy of IsValidIdent from FPC 3.1.
// ToDo: Switch to using IsValidIdent from FPC 3.2 when it is the minimum requirement.
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNum = Alpha + ['0'..'9'];
  Dot = '.';
var
  First: Boolean;
  I, Len: Integer;
begin
  Len := Length(Ident);
  if Len < 1 then
    Exit(False);
  First := True;
  for I := 1 to Len do
  begin
    if First then
    begin
      Result := Ident[I] in Alpha;
      First := False;
    end
    else if AllowDots and (Ident[I] = Dot) then
    begin
      if StrictDots then
      begin
        Result := I < Len;
        First := True;
      end;
    end
    else
      Result := Ident[I] in AlphaNum;
    if not Result then
      Break;
  end;
end;

end.

