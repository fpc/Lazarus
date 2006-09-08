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

{$mode objfpc}
{$H+}

interface

uses
   LazConf, Controls, Forms, SysUtils, Process, Classes, CodeToolManager,
   CodeCache;
   
type
   { TVersionInfo }

   TVersionInfo = class(TObject)
   private
      rcFilename: string;
      resFilename: string;
      rcInFile: text;
      rcOutFile: text;
      rcLine: string;

      procedure BackupRCFile;
      procedure RewriteAndSkipRCFile;
      procedure AppendToRCFile;
      procedure RewriteRCFile;
      function DoTheRealCompile: TModalResult;
      function UpdateMainSourceFile(const AFilename: string): TModalResult;
      function HexToDec(Str: string): Integer;
   public
      TargetOS: string;

      UseVersionInfo: boolean;
      AutoIncrementBuild: boolean;
      VersionNr: integer;
      MajorRevNr: integer;
      MinorRevNr: integer;
      BuildNr: integer;
      HexLang: string;
      HexCharSet: string;
      DescriptionString: string;
      CopyrightString: string;
      CommentsString: string;
      CompanyString: string;
      InternalNameString: string;
      TrademarksString: string;
      OriginalFilenameString: string;
      ProdNameString: string;
      ProductVersionString: string;

      VersionInfoMessages: TStringList;

      Languages: TStringList;            // for Project Options pull-down
      HexLanguages: TStringList;         // for Project Options pull-down

      CharSets: TStringList;             // for Project Options pull-down
      HexCharSets: TStringList;          // for Project Options pull-down

      constructor Create;
      destructor Destroy; override;
      function CompileRCFile(MainFilename: string): TModalResult;
      function SetTargetOS(CurrentProjectsTargetOS: string): string;
      procedure SetFileNames(MainFilename: string);
      procedure SetUseVersionInfo(BoxContents: boolean; var ProjectModified: boolean);
      procedure SetAutoIncrementBuild(BoxContents: boolean; var ProjectModified: boolean);
      procedure SetVersionNr(BoxContents: integer; var ProjectModified: boolean);
      procedure SetMajorRevNr(BoxContents: integer; var ProjectModified: boolean);
      procedure SetMinorRevNr(BoxContents: integer; var ProjectModified: boolean);
      procedure SetBuildNr(BoxContents: integer; var ProjectModified: boolean);
      procedure SetDescriptionString(BoxContents: string; var ProjectModified: boolean);
      procedure SetCopyrightString(BoxContents: string; var ProjectModified: boolean);
      procedure SetHexLang(BoxContents: string; var ProjectModified: boolean);
      procedure SetHexCharSet(BoxContents: string; var ProjectModified: boolean);
      procedure SetCommentsString(BoxContents: string; var ProjectModified: boolean);
      procedure SetCompanyString(BoxContents: string; var ProjectModified: boolean);
      procedure SetInternalNameString(BoxContents: string; var ProjectModified: boolean);
      procedure SetTrademarksString(BoxContents: string; var ProjectModified: boolean);
      procedure SetOriginalFilenameString(BoxContents: string; var ProjectModified: boolean);
      procedure SetProdNameString(BoxContents: string; var ProjectModified: boolean);
      procedure SetProductVersionString(BoxContents: string; var ProjectModified: boolean);
   end;

var
  LocalesList : TStringList;

implementation



{ VersionInfo }

{-----------------------------------------------------------------------------}
{ TVersionInfo Constructor                                                    }
{-----------------------------------------------------------------------------}
constructor TVersionInfo.Create;
begin
   inherited Create;
   VersionInfoMessages := TStringList.Create;
   LocalesList := TStringList.Create;
   Languages := TStringList.Create;
   HexLanguages := TStringList.Create;
   CharSets := TStringList.Create;
   HexCharSets := TStringList.Create;

   Languages.Add('Arabic');                    HexLanguages.Add('0401');
   Languages.Add('Bulgarian');                 HexLanguages.Add('0402');
   Languages.Add('Catalan');                   HexLanguages.Add('0403');
   Languages.Add('Traditional Chinese');       HexLanguages.Add('0404');
   Languages.Add('Czech');                     HexLanguages.Add('0405');
   Languages.Add('Danish');                    HexLanguages.Add('0406');
   Languages.Add('German');                    HexLanguages.Add('0407');
   Languages.Add('Greek');                     HexLanguages.Add('0408');
   Languages.Add('U.S. English');              HexLanguages.Add('0409');
   Languages.Add('Castillian Spanish');        HexLanguages.Add('040A');
   Languages.Add('Finnish');                   HexLanguages.Add('040B');
   Languages.Add('French');                    HexLanguages.Add('040C');
   Languages.Add('Hebrew');                    HexLanguages.Add('040D');
   Languages.Add('Hungarian');                 HexLanguages.Add('040E');
   Languages.Add('Icelandic');                 HexLanguages.Add('040F');
   Languages.Add('Italian');                   HexLanguages.Add('0410');
   Languages.Add('Japanese');                  HexLanguages.Add('0411');
   Languages.Add('Korean');                    HexLanguages.Add('0412');
   Languages.Add('Dutch');                     HexLanguages.Add('0413');
   Languages.Add('Norwegian - Bokmal');        HexLanguages.Add('0414');
   Languages.Add('Swiss Italian');             HexLanguages.Add('0810');
   Languages.Add('Belgian Dutch');             HexLanguages.Add('0813');
   Languages.Add('Norwegian - Nynorsk');       HexLanguages.Add('0814');
   Languages.Add('Polish');                    HexLanguages.Add('0415');
   Languages.Add('Portugese (Brazil)');        HexLanguages.Add('0416');
   Languages.Add('Rhaeto-Romantic');           HexLanguages.Add('0417');
   Languages.Add('Romanian');                  HexLanguages.Add('0418');
   Languages.Add('Russian');                   HexLanguages.Add('0419');
   Languages.Add('Croato-Serbian (Latin)');    HexLanguages.Add('041A');
   Languages.Add('Slovak');                    HexLanguages.Add('041B');
   Languages.Add('Albanian');                  HexLanguages.Add('041C');
   Languages.Add('Swedish');                   HexLanguages.Add('041D');
   Languages.Add('Thai');                      HexLanguages.Add('041E');
   Languages.Add('Turkish');                   HexLanguages.Add('041F');
   Languages.Add('Urdu');                      HexLanguages.Add('0420');
   Languages.Add('Bahasa');                    HexLanguages.Add('0421');
   Languages.Add('Simplified Chinese');        HexLanguages.Add('0804');
   Languages.Add('Swiss German');              HexLanguages.Add('0807');
   Languages.Add('U.K. English');              HexLanguages.Add('0809');
   Languages.Add('Mexican Spanish');           HexLanguages.Add('080A');
   Languages.Add('Belgian French');            HexLanguages.Add('080C');
   Languages.Add('Canadian French');           HexLanguages.Add('0C0C');
   Languages.Add('Swiss French');              HexLanguages.Add('100C');
   Languages.Add('Portugese (Portugal)');      HexLanguages.Add('0816');
   Languages.Add('Sebro-Croatian (Cyrillic)'); HexLanguages.Add('081A');

   { fill charset stringlists }
   CharSets.Add('7-bit ASCII');                HexCharSets.Add('0000');
   CharSets.Add('Japan (Shift - JIS X-0208)'); HexCharSets.Add('03A4');
   CharSets.Add('Korea (Shift - KSC 5601)');   HexCharSets.Add('03B5');
   CharSets.Add('Taiwan (Big5)');              HexCharSets.Add('03B6');
   CharSets.Add('Unicode');                    HexCharSets.Add('04B0');
   CharSets.Add('Latin-2 (Eastern European)'); HexCharSets.Add('04E2');
   CharSets.Add('Cyrillic');                   HexCharSets.Add('04E3');
   CharSets.Add('Multilingual');               HexCharSets.Add('04E4');
   CharSets.Add('Greek');                      HexCharSets.Add('04E5');
   CharSets.Add('Turkish');                    HexCharSets.Add('04E6');
   CharSets.Add('Hebrew');                     HexCharSets.Add('04E7');
   CharSets.Add('Arabic');                     HexCharSets.Add('04E8');

   { initialize version digits }
   VersionNr  := 0;
   MajorRevNr := 0;
   MinorRevNr := 0;
   BuildNr    := 0;

   { initialize strings }
   ProductVersionString   := '';
   CommentsString         := '';
   CompanyString          := '';
   DescriptionString      := '';
   InternalNameString     := '';
   CopyrightString        := '';
   TrademarksString       := '';
   OriginalFilenameString := '';
   ProdNameString         := '';
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo Destructor                                                     }
{-----------------------------------------------------------------------------}
destructor TVersionInfo.Destroy;
begin
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo CompileRCFile                                                  }
{-----------------------------------------------------------------------------}
function TVersionInfo.CompileRCFile(MainFilename: string): TModalResult;
begin
   Result := mrCancel;
   if (GetDefaultTargetOS = 'win32') then
      begin
         { we are building a win32 application }
         if UseVersionInfo then
            begin
               { project indicates to use the versioninfo }
               if (FileExists(rcFilename)) then
                  begin
                     { we found an existing .rc file }
                     if AutoIncrementBuild then
                        begin
                           Inc(BuildNr);
                           RewriteRCFile;
                        end;
                     { now it's time to do the real compile }
                     Result := DoTheRealCompile;
                     if (Result = mrOk) then
                        begin
                           { compilation succeeded }
                           VersionInfoMessages.Clear;
                           VersionInfoMessages.Add('Resource file ' + rcFilename + ' has been compiled successfully!');
                           { we got a compiled .res file, check if it's included in the .lpr file }
                           Result := UpdateMainSourceFile(MainFilename);
                        end
                     else
                        begin
                           { compilation failed }
                           VersionInfoMessages.Add('Errors found while compiling ' + rcFilename);
                        end;
                  end
               else
                  begin
                     { there is no .rc file }
                     VersionInfoMessages.Add(rcFilename + ' does not exist!!!');
                     VersionInfoMessages.Add('Errors found while compiling ' + rcFilename);
                     Result := mrCancel;
                  end;
            end
         else
            begin
               { project indicates to not use the versioninfo }
               Result := mrOk;
            end;
      end
   else
      begin
         { on systems other then win32, there is nothing to do, just return with Result = mrOk }
         Result := mrOk;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo BackupRCFile                                                   }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.BackupRCFile;
begin
   if (FileExists(rcFilename + '.bak')) then
      begin
         { a previous .bak file exists, so erase it }
         AssignFile(rcInFile, rcFilename + '.bak');
         Erase(rcInFile);
      end;
   AssignFile(rcInFile, rcFilename);
   Rename(rcInFile, rcFilename + '.bak');
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo RewriteAndSkipRCFile                                           }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.RewriteAndSkipRCFile;
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

{-----------------------------------------------------------------------------}
{ TVersionInfo AppendToRCFile                                                 }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.AppendToRCFile;
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
 TVersionInfo RewriteRCFile
-----------------------------------------------------------------------------}
procedure TVersionInfo.RewriteRCFile;

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
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo DoTheRealCompile                                               }
{-----------------------------------------------------------------------------}
function TVersionInfo.DoTheRealCompile: TModalResult;
const READ_BYTES = 2048;
var rcProcess: TProcess;
    rcMemStream: TMemoryStream;
    rcStringList: TStringList;
    BytesRead: longint;
    n: longint;
begin
   Result := mrCancel;
   rcMemStream := TMemoryStream.Create;
   BytesRead := 0;
   try
      rcProcess := TProcess.Create(nil);
      rcProcess.CommandLine := 'windres -v ' + rcFilename + ' ' + resFilename;
      rcProcess.ShowWindow := swoHIDE;
      rcProcess.Options := [poUsePipes, poStdErrToOutput];
      rcProcess.Execute;
      while rcProcess.Running do
         begin
            rcMemStream.SetSize(BytesRead + READ_BYTES);
            n := rcProcess.OutPut.Read((rcMemStream.Memory + BytesRead)^, READ_BYTES);
            if n > 0 then
               begin
                  inc(BytesRead, n);
               end
            else
               begin
                  sleep(100);
               end;
         end;
      repeat
         rcMemStream.SetSize(BytesRead + READ_BYTES);
         n := rcProcess.Output.Read((rcMemStream.Memory + BytesRead)^, READ_BYTES);
         if n > 0 then
            begin
               inc(BytesRead, n);
            end;
      until n <= 0;
   finally
      if rcProcess.ExitStatus = 0 then Result := mrOk;
      rcProcess.Free;
   end;
   rcMemStream.SetSize(BytesRead);
   rcStringList := TStringList.Create;
   rcStringList.LoadFromStream(rcMemStream);
   for n := 1 to rcStringList.Count do
      VersionInfoMessages.Add(rcStringList[n - 1]);
end;

{-----------------------------------------------------------------------------
 TVersionInfo UpdateMainSourceFile
-----------------------------------------------------------------------------}
function TVersionInfo.UpdateMainSourceFile(const AFilename: string
   ): TModalResult;
var
   NewX, NewY, NewTopLine: integer;
   VersionInfoCodeBuf: TCodeBuffer;
begin
   Result := mrCancel;
   VersionInfoCodeBuf:=CodeToolBoss.LoadFile(AFilename,false,false);
   if VersionInfoCodeBuf=nil then exit;
   if not CodeToolBoss.FindResourceDirective(VersionInfoCodeBuf,1,1,
                               VersionInfoCodeBuf,NewX,NewY,
                               NewTopLine,ExtractFileName(resFilename)) then
   begin
     if not CodeToolBoss.AddResourceDirective(VersionInfoCodeBuf,
                                              ExtractFileName(resFilename)) then
        begin
           VersionInfoMessages.Add('Could not add "{$R '
                           + ExtractFileName(resFilename)+'}" to main source!');
           exit;
        end
   end;
   Result:=mrOk;
end;

{-----------------------------------------------------------------------------
 TVersionInfo SetTargetOS
-----------------------------------------------------------------------------}
function TVersionInfo.SetTargetOS(CurrentProjectsTargetOS: string): string;
begin
   if CurrentProjectsTargetOS <> '' then
     TargetOS := LowerCase(CurrentProjectsTargetOS)
   else
     TargetOS := LowerCase(GetDefaultTargetOS);
   Result := TargetOS;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetFileNames                                                     }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetFileNames(MainFilename: string);
begin
   rcFilename := Copy(MainFilename, 1, Length(MainFilename) - 4) + '.rc';
   resFilename := Copy(MainFilename, 1, Length(MainFilename) - 4) + '.res';
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetUseVersionInfo                                              }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetUseVersionInfo(BoxContents: boolean;
  var ProjectModified: boolean);
begin
   if (UseVersionInfo <> BoxContents) then
      begin
         UseVersionInfo := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetAutoIncrementBuild                                          }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetAutoIncrementBuild(BoxContents: boolean;
  var ProjectModified: boolean);
begin
   if (AutoIncrementBuild <> BoxContents) then
      begin
         AutoIncrementBuild := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetVersionNr                                                   }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetVersionNr(BoxContents: integer; var ProjectModified: boolean);
begin
   if (VersionNr <> BoxContents) then
      begin
         VersionNr := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetMajorRevNr                                                  }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetMajorRevNr(BoxContents: integer; var ProjectModified: boolean);
begin
   if (MajorRevNr <> BoxContents) then
      begin
         MajorRevNr := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetMinorRevNr                                                  }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetMinorRevNr(BoxContents: integer; var ProjectModified: boolean);
begin
   if (MinorRevNr <> BoxContents) then
      begin
         MinorRevNr := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetBuildNr                                                     }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetBuildNr(BoxContents: integer; var ProjectModified: boolean);
begin
   if (BuildNr <> BoxContents) then
      begin
         BuildNr := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetDescriptionString                                           }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetDescriptionString(BoxContents: string; var ProjectModified: boolean);
begin
   if (DescriptionString <> BoxContents) then
      begin
         DescriptionString := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetCopyrightString                                             }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetCopyrightString(BoxContents: string; var ProjectModified: boolean);
begin
   if (CopyrightString <> BoxContents) then
      begin
         CopyrightString := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetHexLang                                                     }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetHexLang(BoxContents: string; var ProjectModified: boolean);
begin
   BoxContents := HexLanguages[Languages.IndexOf(BoxContents)];
   if (HexLang <> BoxContents) then
      begin
         HexLang := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetHexCharSet                                                  }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetHexCharSet(BoxContents: string; var ProjectModified: boolean);
begin
   BoxContents := HexCharSets[CharSets.IndexOf(BoxContents)];
   if (HexCharSet <> BoxContents) then
      begin
         HexCharSet := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetCommentsString                                              }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetCommentsString(BoxContents: string; var ProjectModified: boolean);
begin
   if (CommentsString <> BoxContents) then
      begin
         CommentsString := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetCompanyString                                               }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetCompanyString(BoxContents: string; var ProjectModified: boolean);
begin
   if (CompanyString <> BoxContents) then
      begin
         CompanyString := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetInternalNameString                                          }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetInternalNameString(BoxContents: string; var ProjectModified: boolean);
begin
   if (InternalNameString <> BoxContents) then
      begin
         InternalNameString := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetTrademarksString                                            }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetTrademarksString(BoxContents: string; var ProjectModified: boolean);
begin
   if (TrademarksString <> BoxContents) then
      begin
         TrademarksString := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetOriginalFilenameString                                      }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetOriginalFilenameString(BoxContents: string; var ProjectModified: boolean);
begin
   if (OriginalFilenameString <> BoxContents) then
      begin
         OriginalFilenameString := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetProdNameString                                              }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetProdNameString(BoxContents: string; var ProjectModified: boolean);
begin
   if (ProdNameString <> BoxContents) then
      begin
         ProdNameString := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo SetProductVersionString                                        }
{-----------------------------------------------------------------------------}
procedure TVersionInfo.SetProductVersionString(BoxContents: string; var ProjectModified: boolean);
begin
   BoxContents := StringReplace(BoxContents, ',', '.', [rfReplaceAll]);
   if (ProductVersionString <> BoxContents) then
      begin
         ProductVersionString := BoxContents;
         ProjectModified := True;
      end;
end;

{-----------------------------------------------------------------------------}
{ TVersionInfo HexToDec                                                       }
{-----------------------------------------------------------------------------}
function TVersionInfo.HexToDec(Str: string): Integer;
var
  i, M: Integer;
begin
  Result:=0;
  M:=1;
  Str:=AnsiUpperCase(Str);
  for i:=Length(Str) downto 1 do
  begin
    case Str[i] of
      '1'..'9': Result:=Result+(Ord(Str[i])-Ord('0'))*M;
      'A'..'F': Result:=Result+(Ord(Str[i])-Ord('A')+10)*M;
    end;
    M:=M shl 4;
  end;
end;

end.

