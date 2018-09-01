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
  Classes, SysUtils,
  // LCL
  StdCtrls, ImgList, Graphics,
  // LazUtils
  LazUTF8, LazFileUtils;

type
  TCmpStrType = (
    cstCaseSensitive,
    cstCaseInsensitive,
    cstFilename
    );

function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
procedure SetComboBoxText(AComboBox: TComboBox; const AText: String;
                          Cmp: TCmpStrType; MaxCount: integer = 1000);
function LoadProjectIconIntoImages(const ProjFile: string;
  const Images: TCustomImageList; const Index: TStringList): Integer;


implementation

function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do begin
    case Cmp of
    cstCaseSensitive:   if List[i]=s then exit(i);
    cstCaseInsensitive: if UTF8CompareText(List[i],s)=0 then exit(i);
    cstFilename:        if CompareFilenames(List[i],s)=0 then exit(i);
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

type
  TLoadProjectIconIntoImagesObject = class
    ImageIndex: Integer;
  end;

function LoadProjectIconIntoImages(const ProjFile: string;
  const Images: TCustomImageList; const Index: TStringList): Integer;
var
  xIconFile: String;
  xIcon: TIcon;
  I: Integer;
  xObj: TLoadProjectIconIntoImagesObject;
begin
  //ToDo: better index

  I := Index.IndexOf(ProjFile);
  if I >= 0 then
    Exit(TLoadProjectIconIntoImagesObject(Index.Objects[I]).ImageIndex);

  if not Index.Sorted or (Index.Count = 0) then
  begin // initialize index
    Index.Sorted := True;
    Index.Duplicates := dupIgnore;
    Index.CaseSensitive := False;
    Index.OwnsObjects := True;
  end;

  Result := -1;
  xIconFile := ChangeFileExt(ProjFile, '.ico');
  if FileExists(xIconFile) then
  begin
    xIcon := TIcon.Create;
    try
      xIcon.LoadFromFile(xIconFile);
      Result := Images.AddIcon(xIcon);
    finally
      xIcon.Free;
    end;
  end;

  xObj := TLoadProjectIconIntoImagesObject.Create;
  xObj.ImageIndex := Result;
  Index.AddObject(ProjFile, xObj);
end;

end.

