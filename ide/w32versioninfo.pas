{
 /***************************************************************************
                        w32versioninfo.pas  -  Lazarus IDE unit
                        ---------------------------------------
                   TVersionInfo is responsible for the inclusion of the 
                   version information in windows executables.


                   Initial Revision  : Sun Feb 20 12:00:00 CST 2006


 ***************************************************************************/

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
unit W32VersionInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, LCLProc, Controls, Forms, FileUtil,
  CodeToolManager, LazConf, ProjectResourcesIntf;
   
type
  { TProjectVersionInfo }

  TProjectVersionInfo = class(TAbstractProjectResource)
  private
    FAutoIncrementBuild: boolean;
    FBuildNr: integer;
    FCommentsString: string;
    FCompanyString: string;
    FCopyrightString: string;
    FDescriptionString: string;
    FHexCharSet: string;
    FHexLang: string;
    FInternalNameString: string;
    FMajorRevNr: integer;
    FMinorRevNr: integer;
    FOriginalFilenameString: string;
    FProdNameString: string;
    FProductVersionString: string;
    FTrademarksString: string;
    FUseVersionInfo: boolean;
    FVersionNr: integer;
    function GetCharSets: TStringList;
    function GetHexCharSets: TStringList;
    function GetHexLanguages: TStringList;
    function GetLanguages: TStringList;
    procedure SetAutoIncrementBuild(const AValue: boolean);
    procedure SetBuildNr(const AValue: integer);
    procedure SetCommentsString(const AValue: string);
    procedure SetCompanyString(const AValue: string);
    procedure SetCopyrightString(const AValue: string);
    procedure SetDescriptionString(const AValue: string);
    procedure SetHexCharSet(const AValue: string);
    procedure SetHexLang(const AValue: string);
    procedure SetInternalNameString(const AValue: string);
    procedure SetMajorRevNr(const AValue: integer);
    procedure SetMinorRevNr(const AValue: integer);
    procedure SetOriginalFilenameString(const AValue: string);
    procedure SetProdNameString(const AValue: string);
    procedure SetProductVersionString(const AValue: string);
    procedure SetTrademarksString(const AValue: string);
    procedure SetUseVersionInfo(const AValue: boolean);
    procedure SetVersionNr(const AValue: integer);
  public
    procedure DoBeforeBuild(AResources: TAbstractProjectResources); override;
    function UpdateResources(AResources: TAbstractProjectResources; const MainFilename: string): Boolean; override;

    property UseVersionInfo: boolean read FUseVersionInfo write SetUseVersionInfo;
    property AutoIncrementBuild: boolean read FAutoIncrementBuild write SetAutoIncrementBuild;
    property VersionNr: integer read FVersionNr write SetVersionNr;
    property MajorRevNr: integer read FMajorRevNr write SetMajorRevNr;
    property MinorRevNr: integer read FMinorRevNr write SetMinorRevNr;
    property BuildNr: integer read FBuildNr write SetBuildNr;
    property HexLang: string read FHexLang write SetHexLang;
    property HexCharSet: string read FHexCharSet write SetHexCharSet;
    property DescriptionString: string read FDescriptionString write SetDescriptionString;
    property CopyrightString: string read FCopyrightString write SetCopyrightString;
    property CommentsString: string read FCommentsString write SetCommentsString;
    property CompanyString: string read FCompanyString write SetCompanyString;
    property InternalNameString: string read FInternalNameString write SetInternalNameString;
    property TrademarksString: string read FTrademarksString write SetTrademarksString;
    property OriginalFilenameString: string read FOriginalFilenameString write SetOriginalFilenameString;
    property ProdNameString: string read FProdNameString write SetProdNameString;
    property ProductVersionString: string read FProductVersionString write SetProductVersionString;
  end;

function MSLanguageToHex(const s: string): string;
function MSHexToLanguage(const s: string): string;
function MSCharacterSetToHex(const s: string): string;
function MSHexToCharacterSet(const s: string): string;

function MSLanguages: TStringList;
function MSHexLanguages: TStringList;
function MSCharacterSets: TStringList;
function MSHexCharacterSets: TStringList;

implementation

var
  // languages
  fLanguages: TStringList = nil;
  fHexLanguages: TStringList = nil;

  // character sets
  fCharSets: TStringList = nil;
  fHexCharSets: TStringList = nil;

procedure CreateCharSets;
begin
  if fCharSets<>nil then exit;
  fCharSets := TStringList.Create;
  fHexCharSets := TStringList.Create;

  fCharSets.Add('7-bit ASCII');                fHexCharSets.Add('0000');
  fCharSets.Add('Japan (Shift - JIS X-0208)'); fHexCharSets.Add('03A4');
  fCharSets.Add('Korea (Shift - KSC 5601)');   fHexCharSets.Add('03B5');
  fCharSets.Add('Taiwan (Big5)');              fHexCharSets.Add('03B6');
  fCharSets.Add('Unicode');                    fHexCharSets.Add('04B0');
  fCharSets.Add('Latin-2 (Eastern European)'); fHexCharSets.Add('04E2');
  fCharSets.Add('Cyrillic');                   fHexCharSets.Add('04E3');
  fCharSets.Add('Multilingual');               fHexCharSets.Add('04E4');
  fCharSets.Add('Greek');                      fHexCharSets.Add('04E5');
  fCharSets.Add('Turkish');                    fHexCharSets.Add('04E6');
  fCharSets.Add('Hebrew');                     fHexCharSets.Add('04E7');
  fCharSets.Add('Arabic');                     fHexCharSets.Add('04E8');
end;

procedure CreateLanguages;
begin
  if fLanguages<>nil then exit;
  fLanguages := TStringList.Create;
  fHexLanguages := TStringList.Create;
  fLanguages.Add('Arabic');                    fHexLanguages.Add('0401');
  fLanguages.Add('Bulgarian');                 fHexLanguages.Add('0402');
  fLanguages.Add('Catalan');                   fHexLanguages.Add('0403');
  fLanguages.Add('Traditional Chinese');       fHexLanguages.Add('0404');
  fLanguages.Add('Czech');                     fHexLanguages.Add('0405');
  fLanguages.Add('Danish');                    fHexLanguages.Add('0406');
  fLanguages.Add('German');                    fHexLanguages.Add('0407');
  fLanguages.Add('Greek');                     fHexLanguages.Add('0408');
  fLanguages.Add('U.S. English');              fHexLanguages.Add('0409');
  fLanguages.Add('Castillian Spanish');        fHexLanguages.Add('040A');
  fLanguages.Add('Finnish');                   fHexLanguages.Add('040B');
  fLanguages.Add('French');                    fHexLanguages.Add('040C');
  fLanguages.Add('Hebrew');                    fHexLanguages.Add('040D');
  fLanguages.Add('Hungarian');                 fHexLanguages.Add('040E');
  fLanguages.Add('Icelandic');                 fHexLanguages.Add('040F');
  fLanguages.Add('Italian');                   fHexLanguages.Add('0410');
  fLanguages.Add('Japanese');                  fHexLanguages.Add('0411');
  fLanguages.Add('Korean');                    fHexLanguages.Add('0412');
  fLanguages.Add('Dutch');                     fHexLanguages.Add('0413');
  fLanguages.Add('Norwegian - Bokmal');        fHexLanguages.Add('0414');
  fLanguages.Add('Swiss Italian');             fHexLanguages.Add('0810');
  fLanguages.Add('Belgian Dutch');             fHexLanguages.Add('0813');
  fLanguages.Add('Norwegian - Nynorsk');       fHexLanguages.Add('0814');
  fLanguages.Add('Polish');                    fHexLanguages.Add('0415');
  fLanguages.Add('Portugese (Brazil)');        fHexLanguages.Add('0416');
  fLanguages.Add('Rhaeto-Romantic');           fHexLanguages.Add('0417');
  fLanguages.Add('Romanian');                  fHexLanguages.Add('0418');
  fLanguages.Add('Russian');                   fHexLanguages.Add('0419');
  fLanguages.Add('Croato-Serbian (Latin)');    fHexLanguages.Add('041A');
  fLanguages.Add('Slovak');                    fHexLanguages.Add('041B');
  fLanguages.Add('Albanian');                  fHexLanguages.Add('041C');
  fLanguages.Add('Swedish');                   fHexLanguages.Add('041D');
  fLanguages.Add('Thai');                      fHexLanguages.Add('041E');
  fLanguages.Add('Turkish');                   fHexLanguages.Add('041F');
  fLanguages.Add('Urdu');                      fHexLanguages.Add('0420');
  fLanguages.Add('Bahasa');                    fHexLanguages.Add('0421');
  fLanguages.Add('Simplified Chinese');        fHexLanguages.Add('0804');
  fLanguages.Add('Swiss German');              fHexLanguages.Add('0807');
  fLanguages.Add('U.K. English');              fHexLanguages.Add('0809');
  fLanguages.Add('Mexican Spanish');           fHexLanguages.Add('080A');
  fLanguages.Add('Belgian French');            fHexLanguages.Add('080C');
  fLanguages.Add('Canadian French');           fHexLanguages.Add('0C0C');
  fLanguages.Add('Swiss French');              fHexLanguages.Add('100C');
  fLanguages.Add('Portugese (Portugal)');      fHexLanguages.Add('0816');
  fLanguages.Add('Sebro-Croatian (Cyrillic)'); fHexLanguages.Add('081A');
end;

function MSLanguageToHex(const s: string): string;
var
  i: LongInt;
begin
  i:=MSLanguages.IndexOf(s);
  if i>=0 then
    Result:=fHexLanguages[i]
  else
    Result:='';
end;

function MSHexToLanguage(const s: string): string;
var
  i: LongInt;
begin
  i:=MSHexLanguages.IndexOf(s);
  if i>=0 then
    Result:=fLanguages[i]
  else
    Result:='';
end;

function MSCharacterSetToHex(const s: string): string;
var
  i: LongInt;
begin
  i:=MSCharacterSets.IndexOf(s);
  if i>=0 then
    Result:=fHexCharSets[i]
  else
    Result:='';
end;

function MSHexToCharacterSet(const s: string): string;
var
  i: LongInt;
begin
  i:=MSHexCharacterSets.IndexOf(s);
  if i>=0 then
    Result:=fCharSets[i]
  else
    Result:='';
end;

function MSLanguages: TStringList;
begin
  CreateLanguages;
  Result:=fLanguages;
end;

function MSHexLanguages: TStringList;
begin
  CreateLanguages;
  Result:=fHexLanguages;
end;

function MSCharacterSets: TStringList;
begin
  CreateCharSets;
  Result:=fCharSets;
end;

function MSHexCharacterSets: TStringList;
begin
  CreateCharSets;
  Result:=fHexCharSets;
end;

{ VersionInfo }

function TProjectVersionInfo.UpdateResources(AResources: TAbstractProjectResources;
  const MainFilename: string): Boolean;
   { File structure
    A rc file can contain several pieces of information. One of those pieces
    is the version information. The version information is a block of data
    the following form:
       <version-id> VERSIONINFO <fixed-info> [ <block-statement> ... ]
    <version-id> must always be 1. <fixed-info> can be one or more of the
    following key-words followed by a parameter:
       FILEVERSION <version>
       PRODUCTVERSION <version>
       FILEFLAGSMASK <fileflagsmask>     *
       FILEFLAGS <fileflags>             *
       FILEOS <fileos>                   *
       FILETYPE <filetype>               *
       FILESUBTYPE <subtype>             *
    In this routine only FILEVERSION and PRODUCTVERSION are used. Between
    the parantheses 2 blocks can be used:
       STRINGFILEINFO
       VARFILEINFO
    The format for the STRINGFILEINFO is:
       BLOCK "StringFileInfo" [ BLOCK "<lang><charset>" [ VALUE
          "<string-name>", "<value>" ...] ]
    <lang> is the language code notated as 4 digit hex value. <charset> is
    the character set code also in 4 digit hex. Possble string-names are:
       Comments
       CompanyName
       FileDescription
       FileVersion
       InternalName
       LegalCopyright
       LegalTrademarks
       OriginalFilename
       PrivateBuild      *
       ProductName
       ProductVersion
       SpecialBuild      *
    In this routine all values except PrivateBuild and SpecialBuild are
    used. The format for the VARFILEINFO is:
       BLOCK "VarFileInfo" [ VALUE "Translation", <lang>, <charset> ... ]
    <lang> and <charset> are the same hex values as in StringFileInfo, but
    here they "0x" needs to be put in front of the value.
}
var
  AList: TStringList;
begin
  Result := True;
  if UseVersionInfo then
  begin
    // project indicates to use the versioninfo
    if ProductVersionString = '' then
       ProductVersionString := IntToStr(VersionNr) + '.' +
                               IntToStr(MajorRevNr) + '.' +
                               IntToStr(MinorRevNr) + '.' +
                               IntToStr(BuildNr);
    // todo: improve this
    AList := TStringList.Create;
    AList.Add('1 VERSIONINFO');
    AList.Add('FILEVERSION ' + IntToStr(VersionNr) + ',' +
                               IntToStr(MajorRevNr) + ',' +
                               IntToStr(MinorRevNr) + ',' +
                               IntToStr(BuildNr));
    AList.Add('PRODUCTVERSION ' + StringReplace(ProductVersionString, '.', ',', [rfReplaceAll]));
    AList.Add('{');
    AList.Add(' BLOCK "StringFileInfo"');
    AList.Add(' {');
    AList.Add('  BLOCK "' + HexLang + HexCharSet + '"');
    AList.Add('  {');
    AList.Add('   VALUE "Comments", "' + CommentsString + '\000"');
    AList.Add('   VALUE "CompanyName", "' + CompanyString + '\000"');
    AList.Add('   VALUE "FileDescription", "' + DescriptionString + '\000"');
    AList.Add('   VALUE "FileVersion", "' + IntToStr(VersionNr) + '.' +
                                            IntToStr(MajorRevNr) + '.' +
                                            IntToStr(MinorRevNr) + '.' +
                                            IntToStr(BuildNr) + '\000"');
    AList.Add('   VALUE "InternalName", "' + InternalNameString + '\000"');
    AList.Add('   VALUE "LegalCopyright", "' + CopyrightString + '\000"');
    AList.Add('   VALUE "LegalTrademarks", "' + TrademarksString + '\000"');
    AList.Add('   VALUE "OriginalFilename", "' + OriginalFilenameString + '\000"');
    AList.Add('   VALUE "ProductName", "' + ProdNameString + '\000"');
    AList.Add('   VALUE "ProductVersion", "' +
              StringReplace(ProductVersionString, ',', '.', [rfReplaceAll]) +
              '\000"');
    AList.Add('  }');
    AList.Add(' }');
    AList.Add(' BLOCK "VarFileInfo"');
    AList.Add(' {');
    AList.Add('  VALUE "Translation", 0x' + HexLang + ', 0x' + HexCharSet);
    AList.Add(' }');
    AList.Add('}');
    AResources.AddSystemResource(AList.Text);
    AList.Free;
  end;
end;

function TProjectVersionInfo.GetCharSets: TStringList;
begin
  CreateCharSets;
  Result:=fHexCharSets;
end;

function TProjectVersionInfo.GetHexCharSets: TStringList;
begin
  CreateCharSets;
  Result:=fHexCharSets;
end;

function TProjectVersionInfo.GetHexLanguages: TStringList;
begin
  CreateLanguages;
  Result:=fHexLanguages;
end;

function TProjectVersionInfo.GetLanguages: TStringList;
begin
  CreateLanguages;
  Result:=fLanguages;
end;

procedure TProjectVersionInfo.SetAutoIncrementBuild(const AValue: boolean);
begin
  if FAutoIncrementBuild=AValue then exit;
  FAutoIncrementBuild:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetBuildNr(const AValue: integer);
begin
  if FBuildNr=AValue then exit;
  FBuildNr:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetCommentsString(const AValue: string);
begin
  if FCommentsString=AValue then exit;
  FCommentsString:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetCompanyString(const AValue: string);
begin
  if FCompanyString=AValue then exit;
  FCompanyString:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetCopyrightString(const AValue: string);
begin
  if FCopyrightString=AValue then exit;
  FCopyrightString:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetDescriptionString(const AValue: string);
begin
  if FDescriptionString=AValue then exit;
  FDescriptionString:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetHexCharSet(const AValue: string);
begin
  if FHexCharSet=AValue then exit;
  FHexCharSet:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetHexLang(const AValue: string);
begin
  if FHexLang=AValue then exit;
  FHexLang:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetInternalNameString(const AValue: string);
begin
  if FInternalNameString=AValue then exit;
  FInternalNameString:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetMajorRevNr(const AValue: integer);
begin
  if FMajorRevNr=AValue then exit;
  FMajorRevNr:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetMinorRevNr(const AValue: integer);
begin
  if FMinorRevNr=AValue then exit;
  FMinorRevNr:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetOriginalFilenameString(const AValue: string);
begin
  if FOriginalFilenameString=AValue then exit;
  FOriginalFilenameString:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetProdNameString(const AValue: string);
begin
  if FProdNameString=AValue then exit;
  FProdNameString:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetProductVersionString(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=StringReplace(AValue, ',', '.', [rfReplaceAll]);
  if FProductVersionString=NewValue then exit;
  FProductVersionString:=NewValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetTrademarksString(const AValue: string);
begin
  if FTrademarksString=AValue then exit;
  FTrademarksString:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetUseVersionInfo(const AValue: boolean);
begin
  if FUseVersionInfo=AValue then exit;
  FUseVersionInfo:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.SetVersionNr(const AValue: integer);
begin
  if FVersionNr=AValue then exit;
  FVersionNr:=AValue;
  Modified:=true;
end;

procedure TProjectVersionInfo.DoBeforeBuild(
  AResources: TAbstractProjectResources);
begin
  if AutoIncrementBuild then // project indicate to use autoincrementbuild
    BuildNr := BuildNr + 1;
end;

finalization
  FreeAndNil(fHexCharSets);
  FreeAndNil(fHexLanguages);
  FreeAndNil(fLanguages);
  FreeAndNil(fCharSets);

end.

