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
  Classes, SysUtils, Process, LCLProc, Controls, Forms,
  CodeToolManager, CodeCache, LazConf;
   
type
  { TProjectVersionInfo }

  TProjectVersionInfo = class(TObject)
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
    FModified: boolean;
    FOnModified: TNotifyEvent;
    FOriginalFilenameString: string;
    FProdNameString: string;
    FProductVersionString: string;
    FTrademarksString: string;
    FUseVersionInfo: boolean;
    FVersionNr: integer;
    rcFilename: string;
    rcInFile: text;
    rcOutFile: text;
    rcLine: string;
    fVersionInfoMessages: TStringList;
    procedure BackupRCFile;
    function GetCharSets: TStringList;
    function GetHexCharSets: TStringList;
    function GetHexLanguages: TStringList;
    function GetLanguages: TStringList;
    function GetVersionInfoMessages: TStringList;
    procedure RewriteAndSkipRCFile;
    procedure AppendToRCFile;
    procedure RewriteRCFile;
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
    procedure SetModified(const AValue: boolean);
    procedure SetOriginalFilenameString(const AValue: string);
    procedure SetProdNameString(const AValue: string);
    procedure SetProductVersionString(const AValue: string);
    procedure SetTrademarksString(const AValue: string);
    procedure SetUseVersionInfo(const AValue: boolean);
    procedure SetVersionNr(const AValue: integer);
    procedure SetFileNames(const MainFilename: string);
  public
    constructor Create;
    destructor Destroy; override;
    function CreateRCFile(const MainFilename, TargetOS: string): TModalResult;
    function UpdateMainSourceFile(const AFilename: string): TModalResult;

    property Modified: boolean read FModified write SetModified;

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
    property VersionInfoMessages: TStringList read GetVersionInfoMessages;
    
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
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

{-----------------------------------------------------------------------------
 TProjectVersionInfo Constructor
-----------------------------------------------------------------------------}
constructor TProjectVersionInfo.Create;
begin
  inherited Create;
end;

{-----------------------------------------------------------------------------
 TProjectVersionInfo Destructor
-----------------------------------------------------------------------------}
destructor TProjectVersionInfo.Destroy;
begin
  FreeAndNil(fVersionInfoMessages);
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
 TProjectVersionInfo CreateRCFile
-----------------------------------------------------------------------------}
function TProjectVersionInfo.CreateRCFile(const MainFilename, TargetOS: string
  ): TModalResult;
begin
  Result := mrCancel;
  SetFileNames(MainFilename);
  if (TargetOS = 'win32') then
  begin
    // we are building a win32 application
    if UseVersionInfo then
    begin
      // project indicates to use the versioninfo
      if AutoIncrementBuild then // project indicate to use autoincrementbuild
        BuildNr := BuildNr + 1;
      if ProductVersionString = '' then
         ProductVersionString := IntToStr(VersionNr) + '.' +
                                 IntToStr(MajorRevNr) + '.' +
                                 IntToStr(MinorRevNr) + '.' +
                                 IntToStr(BuildNr);
      if (FileExists(rcFilename)) then // we found an existing .rc file
        RewriteRCFile
      else
      begin
        // there is no .rc file
        AssignFile(rcOutFile, rcFilename);
        Rewrite(rcOutFile);
        AppendToRCFile;
        CloseFile(rcOutFile);
      end;
      Result := mrOk;
    end
    else
    begin
      // project indicates to not use the versioninfo
      Result := mrOk;
    end;
  end
  else
  begin
    // on systems other then win32, there is nothing to do, just return with Result = mrOk
    Result := mrOk;
  end;
end;

{-----------------------------------------------------------------------------
 TProjectVersionInfo BackupRCFile
-----------------------------------------------------------------------------}
procedure TProjectVersionInfo.BackupRCFile;
begin
  //BackupFileInteractive(rcFilename);
   if (FileExists(rcFilename + '.bak')) then
      begin
         { a previous .bak file exists, so erase it }
         AssignFile(rcInFile, rcFilename + '.bak');
         Erase(rcInFile);
      end;
   AssignFile(rcInFile, rcFilename);
   Rename(rcInFile, rcFilename + '.bak');
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

function TProjectVersionInfo.GetVersionInfoMessages: TStringList;
begin
  if fVersionInfoMessages=nil then
    fVersionInfoMessages:=TStringList.Create;
  Result:=fVersionInfoMessages;
end;

{-----------------------------------------------------------------------------
 TProjectVersionInfo RewriteAndSkipRCFile
-----------------------------------------------------------------------------}
procedure TProjectVersionInfo.RewriteAndSkipRCFile;
var Stage : integer;
begin
   Stage := 0;   { 0 = no versioninfo found yet                               }
                 { 1 = '1 VERSIONINFO' was found                              }
                 { 2 = opening parantheses for main block was found           }
                 { 3 = opening parantheses for StringFileInfo od VarFileInfo  }
                 {     was found                                              }
                 { 4 = internal StringFileInfo block was found                }
   while not(eof(rcInFile)) do
      begin
         ReadLn(rcInFile, rcLine);
         if ((Copy(TrimLeft(rcLine), 1, 13) = '1 VERSIONINFO') and (Stage = 0)) then
            begin
               { we found the "1 VERSIONINFO" line                            }
               Stage := 1;
            end;
         if (Stage = 0) then
            begin
               { this is a non-versioninfo line, just write it out            }
               WriteLn(rcOutFile, rcLine);
            end;
         if (Copy(TrimLeft(rcLine), 1, 1) = '{') then
            begin
               Case Stage of
                  1 : Stage := 2;   // opening { for main block
                  2 : Stage := 3;   // opening { for either StringFileInfo or VarFileInfo
                  3 : Stage := 4;   // opening { for internal StringFileInfo block
               end;
            end;
         if (Copy(TrimLeft(rcLine), 1, 1) = '}') then
            begin
               Case Stage of
                  4 : Stage := 3;   // closing } for internal StringFileInfo block
                  3 : Stage := 2;   // closing } for either StringFileInfo or VarFileInfo
                  2 : Stage := 0;   // closing } for main block
               end;
            end;
      end;
end;

{-----------------------------------------------------------------------------
 TProjectVersionInfo AppendToRCFile
-----------------------------------------------------------------------------}
procedure TProjectVersionInfo.AppendToRCFile;
begin
  rcLine := '1 VERSIONINFO';
  WriteLn(rcoutFile, rcLine);
  rcLine := 'FILEVERSION ' + IntToStr(VersionNr) + ',' +
                             IntToStr(MajorRevNr) + ',' +
                             IntToStr(MinorRevNr) + ',' +
                             IntToStr(BuildNr);
  WriteLn(rcoutFile, rcLine);
  rcLine := 'PRODUCTVERSION ' + StringReplace(ProductVersionString, '.', ',', [rfReplaceAll]);
  WriteLn(rcoutFile, rcLine);
  rcLine := '{';
  WriteLn(rcoutFile, rcLine);
  rcLine := ' BLOCK "StringFileInfo"';
  WriteLn(rcoutFile, rcLine);
  rcLine := ' {';
  WriteLn(rcoutFile, rcLine);
  rcLine := '  BLOCK "' + HexLang + HexCharSet + '"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '  {';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "Comments", "' + CommentsString + '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "CompanyName", "' + CompanyString + '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "FileDescription", "' + DescriptionString + '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "FileVersion", "' + IntToStr(VersionNr) + '.' +
                                          IntToStr(MajorRevNr) + '.' +
                                          IntToStr(MinorRevNr) + '.' +
                                          IntToStr(BuildNr) + '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "InternalName", "' + InternalNameString + '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "LegalCopyright", "' + CopyrightString + '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "LegalTrademarks", "' + TrademarksString + '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "OriginalFilename", "' + OriginalFilenameString + '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "ProductName", "' + ProdNameString + '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '   VALUE "ProductVersion", "' +
            StringReplace(ProductVersionString, ',', '.', [rfReplaceAll]) +
            '\000"';
  WriteLn(rcoutFile, rcLine);
  rcLine := '  }';
  WriteLn(rcoutFile, rcLine);
  rcLine := ' }';
  WriteLn(rcoutFile, rcLine);
  rcLine := ' BLOCK "VarFileInfo"';
  WriteLn(rcoutFile, rcLine);
  rcLine := ' {';
  WriteLn(rcoutFile, rcLine);
  rcLine := '  VALUE "Translation", 0x' + HexLang + ', 0x' + HexCharSet;
  WriteLn(rcoutFile, rcLine);
  rcLine := ' }';
  WriteLn(rcoutFile, rcLine);
  rcLine := '}';
  WriteLn(rcoutFile, rcLine);
end;

{-----------------------------------------------------------------------------
 TProjectVersionInfo RewriteRCFile
-----------------------------------------------------------------------------}
procedure TProjectVersionInfo.RewriteRCFile;

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

    This routine does the following:
       1 - backup the rc file
       2 - read from the backed up file and write all non-version-info lines
           to the new rc file. All version-info related lines are left out.
       3 - append the new version-info to the rc file.                       }

begin
  try
    { first make a backup of the current rc file }
    BackupRCFile;

    { open the backup for input and create a new file for output }
    AssignFile(rcInFile, rcFilename + '.bak');
    Reset(rcInFile);
    AssignFile(rcOutFile, rcFilename);
    Rewrite(rcOutFile);

    { read from input, skip all version-info and write back the remainder }
    RewriteAndSkipRCFile;

    { append the new version-info }
    AppendToRCFile;

    { close the files }
    CloseFile(rcInFile);
    CloseFile(rcOutFile);
  except
    on E: Exception do begin
      DebugLn(['TProjectVersionInfo.RewriteRCFile ',e.Message]);
    end;
  end;
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

procedure TProjectVersionInfo.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  if Assigned(OnModified) then OnModified(Self);
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

{-----------------------------------------------------------------------------
 TProjectVersionInfo UpdateMainSourceFile
-----------------------------------------------------------------------------}
function TProjectVersionInfo.UpdateMainSourceFile(const AFilename: string
  ): TModalResult;
var
  NewX, NewY, NewTopLine: integer;
  VersionInfoCodeBuf: TCodeBuffer;
  Filename: String;
  NewCode: TCodeBuffer;
begin
  Result := mrCancel;
  VersionInfoCodeBuf:=CodeToolBoss.LoadFile(AFilename,false,false);
  if VersionInfoCodeBuf=nil then exit;
  SetFileNames(AFilename);
  Filename:=ExtractFileName(rcFilename);
  //DebugLn(['TProjectVersionInfo.UpdateMainSourceFile ',Filename,' UseVersionInfo=',UseVersionInfo]);
  if CodeToolBoss.FindResourceDirective(VersionInfoCodeBuf,1,1,
                               NewCode,NewX,NewY,
                               NewTopLine,Filename,false) then
  begin
    if not UseVersionInfo then begin
      //DebugLn(['TProjectVersionInfo.UpdateMainSourceFile removing ',NewCode.Filename,' X=',NewX,' Y=',NewY]);
      if not CodeToolBoss.RemoveDirective(NewCode,NewX,NewY,true) then
      begin
        DebugLn(['TProjectVersionInfo.UpdateMainSourceFile FAILED removing']);
        VersionInfoMessages.Add('Could not emove "{$R '
                             + Filename+'}" from main source!');
        exit;
      end
    end;
  end else if UseVersionInfo then begin
    //DebugLn(['TProjectVersionInfo.UpdateMainSourceFile adding ',AFilename]);
    if not CodeToolBoss.AddResourceDirective(VersionInfoCodeBuf,
      Filename,false,'{$IFDEF WINDOWS}{$R '+Filename+'}{$ENDIF}') then
    begin
      DebugLn(['TProjectVersionInfo.UpdateMainSourceFile FAILED adding']);
      VersionInfoMessages.Add('Could not add "{$R '
                           + Filename+'}" to main source!');
      exit;
    end
  end;
  //DebugLn(['TProjectVersionInfo.UpdateMainSourceFile ',VersionInfoCodeBuf.Source]);
  Result:=mrOk;
end;

{-----------------------------------------------------------------------------
 TProjectVersionInfo SetFileNames
-----------------------------------------------------------------------------}
procedure TProjectVersionInfo.SetFileNames(const MainFilename: string);
begin
  rcFilename := Copy(MainFilename, 1, Length(MainFilename) - 4) + '.rc';
end;

finalization
  FreeAndNil(fHexCharSets);
  FreeAndNil(fHexLanguages);
  FreeAndNil(fLanguages);
  FreeAndNil(fCharSets);

end.

