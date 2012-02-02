{
 /***************************************************************************
                              idecmdline.pas
                             --------------------
               A unit to manage command lines issue used inside the ide

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
 
 Author: Ido Kanner

  This unit manages the commandline utils that are used across Lazarus.
  It was created for avoding duplicates and easier access for commandline utils
  that are required by the IDE.
}
unit IDECmdLine;

{$mode objfpc}{$H+}

interface

uses 
  Classes, SysUtils, FileUtil, LazConf, LCLProc;

const
  ShowSetupDialogOptLong='--setup';
  PrimaryConfPathOptLong='--primary-config-path=';
  PrimaryConfPathOptShort='--pcp=';
  SecondaryConfPathOptLong='--secondary-config-path=';
  SecondaryConfPathOptShort='--scp=';
  NoSplashScreenOptLong='--no-splash-screen';
  NoSplashScreenOptShort='--nsc';
  StartedByStartLazarusOpt='--started-by-startlazarus';
  SkipLastProjectOpt='--skip-last-project';
  DebugLogOpt='--debug-log=';
  LanguageOpt='--language=';
  LazarusDirOpt ='--lazarusdir=';

procedure ParseCommandLine(aCmdLineParams : TStrings; out IDEPid : Integer;
            out ShowSplashScreen: boolean);
function GetCommandLineParameters(aCmdLineParams : TStrings;
            isStartLazarus : Boolean = False) : String;

function IsHelpRequested : Boolean;
function IsVersionRequested : boolean;
function GetLanguageSpecified : string;
function ParamIsOption(ParamIndex : integer; const Option : string) : boolean;
function ParamIsOptionPlusValue(ParamIndex : integer;
            const Option : string; out AValue : string) : boolean;

procedure ParseNoGuiCmdLineParams;

function ExtractCmdLineFilenames : TStrings;

function GetLazarusDirectory : String;

implementation

procedure ParseCommandLine(aCmdLineParams: TStrings; out IDEPid: Integer; out
  ShowSplashScreen: boolean);
const
  LazarusPidOpt   = '--lazarus-pid=';
  LazarusDebugOpt = '--debug';
var
  i     : Integer;
  Param : string;
begin
  IDEPid := 0;
  for i := 1 to ParamCount do begin
    Param := ParamStrUTF8(i);
    if Param=LazarusDebugOpt then begin
      aCmdLineParams.Add('--debug-log=' +
                         AppendPathDelim(UTF8ToSys(GetPrimaryConfigPath)) + 'debug.log');
    end;
    if LeftStr(Param,length(LazarusPidOpt))=LazarusPidOpt then begin
      try
        IDEPid :=
          StrToInt(RightStr(Param,Length(Param)-Length(LazarusPidOpt)));
      except
        DebugLn('Failed to parse %s',[Param]);
        IDEPid := 0;
      end;
    end
    else if ParamIsOption(i, NoSplashScreenOptLong) or
            ParamIsOption(i, NoSplashScreenOptShort)    then
       begin
         ShowSplashScreen := false;
       end
    else
      begin
        // pass these parameters to Lazarus

        if LeftStr(Param,length(PrimaryConfPathOptShort))=PrimaryConfPathOptShort
        then begin
          SetPrimaryConfigPath(copy(Param,length(PrimaryConfPathOptShort)+1,length(Param)));
        end;

        // Do not add file to the parameter list
        if not (Copy(Param,1,1) = '-') and (FileExistsUTF8(ExpandFileNameUTF8(Param))) then
          begin
            DebugLn('%s is a file', [Param]);
            continue;
          end;
          
        DebugLn('Adding "%s" as a parameter', [Param]);
        aCmdLineParams.Add(Param);
      end;
  end;
  // make sure that command line parameters are still
  // double quoted, if they contain spaces
  for i := 0 to aCmdLineParams.Count -1 do
  begin
    if pos(' ',aCmdLineParams[i])>0 then
      aCmdLineParams[i] := '"' + aCmdLineParams[i] + '"';
  end;
end;

function GetCommandLineParameters(aCmdLineParams : TStrings; isStartLazarus : Boolean = False) : String;
var
  i: Integer;
begin
  if isStartLazarus then
    Result := ' --no-splash-screen --started-by-startlazarus'
  else
    Result := '';
  for i := 0 to aCmdLineParams.Count - 1 do
    Result := Result + ' ' + aCmdLineParams[i];
end;

function IsHelpRequested : Boolean;
var
  i: integer;
begin
  Result := false;
  i:=1;
  while (i <= ParamCount) and (Result = false) do
  begin
    Result := ParamIsOption(i, '--help') or
              ParamIsOption(i, '-help')  or
              ParamIsOption(i, '-?')     or
              ParamIsOption(i, '-h');
    inc(i);
  end;
end;

function IsVersionRequested: boolean;
begin
  Result := (ParamCount=1) and
            (ParamIsOption(1, '--version') or
             ParamIsOption(1, '-v'));
end;

function GetLanguageSpecified : string;
var
  i: integer;
  AValue: string;
begin
  // return language specified in command line (empty string if no language specified)
  Result := '';
  AValue := '';
  i := 1;
  while i <= ParamCount do
  begin
    if ParamIsOptionPlusValue(i, LanguageOpt, AValue) = true then
    begin
      Result := AValue;
      exit;
    end;
    inc(i);
  end;
end;

function ParamIsOption(ParamIndex : integer; const Option : string) : boolean;
begin
  Result:=SysUtils.CompareText(ParamStrUTF8(ParamIndex),Option) = 0;
end;

function ParamIsOptionPlusValue(ParamIndex : integer;
    const Option : string; out AValue : string) : boolean;
var
  p : String;
begin
 p      := ParamStrUTF8(ParamIndex);
 Result := SysUtils.CompareText(LeftStr(p, length(Option)), Option) = 0;
 if Result then
   AValue := copy(p, length(Option) + 1, length(p))
 else
   AValue := '';
end;

procedure ParseNoGuiCmdLineParams;
var
  i      : integer;
  AValue : String;
begin
  for i:= 1 to ParamCount do
  begin
    //DebugLn(['ParseNoGuiCmdLineParams ',i,' "',ParamStrUTF8(i),'"']);
    if ParamIsOptionPlusValue(i, PrimaryConfPathOptLong, AValue) then
      SetPrimaryConfigPath(AValue)
    else if ParamIsOptionPlusValue(i, PrimaryConfPathOptShort, AValue) then
      SetPrimaryConfigPath(AValue)
    else if ParamIsOptionPlusValue(i, SecondaryConfPathOptLong, AValue) then
      SetSecondaryConfigPath(AValue)
    else if ParamIsOptionPlusValue(i, SecondaryConfPathOptShort, AValue) then
      SetSecondaryConfigPath(AValue);
  end;
end;

function ExtractCmdLineFilenames : TStrings;
var
  i        : LongInt;
  Filename : String;
  
begin
  Result := nil;
  for i := 1 to ParamCount do
   begin
     Filename := ParamStrUTF8(i);
     if (Filename = '') or (Filename[1] = '-') then
       continue;
     if Result = nil then
       Result := TStringList.Create;
     Result.Add(Filename);
    end;
end;

function GetLazarusDirectory : String;
begin
  Result := ExtractFileDir(ParamStrUTF8(0));
end;

end.

