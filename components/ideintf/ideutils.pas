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
  if AText<>'' then begin
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
  end;
  AComboBox.Text := AText;
end;

end.

