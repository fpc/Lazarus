{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Werner Pamler

- If LazFreeType does not find the fonts needed call InitFonts at the beginning
  of the program and specify the path to the font folder as a parameter.
  Several folders can be used if separated by LineEnding codes.
}


unit TAFonts;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, EasyLazFreeType;

procedure InitFonts(AFontDir: string = '');
function LoadFont(AFontName: String; AStyle: TFreeTypeStyles): TFreeTypeFont;


implementation

uses
  FileUtil, LazFileUtils, LazFreeTypeFontCollection;

var
  FontsInitialized: Boolean = false;

procedure PopulateFontDirList(AList: TStrings);
const
  CSIDL_FONTS = 20;
var
  s: String;
begin
  if AList = nil then
    raise Exception.Create('PopulateFontDirList: list not allocated.');

 {$IFDEF WINDOWS}
  s := SHGetFolderPathUTF8(CSIDL_FONTS);
  if s <> '' then
    AList.Add(s);
 {$ENDIF}
 {$IFDEF linux}
  AList.Add('/usr/share/cups/fonts/');
  AList.Add('/usr/share/fonts/truetype/');
  AList.Add('/usr/local/lib/X11/fonts/');
  AList.Add(GetUserDir + '.fonts/');
 {$ENDIF}
 {$IFDEF LCLCarbon}
  AList.Add('/Library/Fonts/');
  AList.Add('/System/Library/Fonts/');
  AList.Add('/Network/Library/Fonts/');
  AList.Add('~/Library/Fonts/');
 {$ENDIF}
 {$IFDEF LCLCocoa}
  AList.Add('/Library/Fonts/');
  AList.Add('/System/Library/Fonts/');
  AList.Add('/Network/Library/Fonts/');
  AList.Add('~/Library/Fonts/');
 {$ENDIF}
end;

function LoadFont(AFontName: String; AStyle: TFreeTypeStyles): TFreeTypeFont;
var
  familyItem: TCustomFamilyCollectionItem;
  fontItem: TCustomFontCollectionItem;
  style: String;
begin
  Result := nil;
  familyItem := FontCollection.Family[AFontName];
  if familyItem <> nil then begin
    style := '';
    if (ftsBold in AStyle) then style := 'Bold';
    if (ftsItalic in AStyle) then style := style + ' Italic';
    fontItem := familyItem.GetFont(style);
    if fontItem <> nil then begin
      Result := fontItem.CreateFont;
      Result.Style := AStyle;
    end;
  end;
end;

procedure InitFonts(AFontDir: String = '');

  { Duplicates functionality in FontCollection.AddFolder in order to be able to
    ignore exceptions due to font read errors (occur on Linux Mint with font
    NanumMyeongjo.ttf) }
  procedure AddFolder(AFolder: string);
  var
    files: TStringList;
    i: integer;
  begin
    AFolder := ExpandFileName(AFolder);
    if (length(AFolder) <> 0) and (AFolder[length(AFolder)] <> PathDelim) then
      AFolder += PathDelim;
    files := TStringList.Create;
    FontCollection.BeginUpdate;
    try
      FindAllFiles(files, AFolder, '*.ttf', true);
      files.Sort;
      for i := 0 to files.Count-1 do
        try
          FontCollection.AddFile(files[i]);
        except
        end;
    finally
      FontCollection.EndUpdate;
      files.Free;
    end;
  end;

var
  i: Integer;
  fontDirList: TStrings;
begin
  if FontsInitialized then
    exit;

  fontDirList := TStringList.Create;
  try
    PopulateFontDirList(fontDirList);
    if AFontDir <> '' then
      fontDirList.Text := AFontDir;
    for i:=0 to fontDirList.Count-1 do
      AddFolder(fontDirList[i]);
    FontsInitialized := true;
  finally
    fontDirList.Free;
  end;
end;

end.
