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
  Classes, SysUtils, FileUtil, LazConf, LCLProc, LazarusIDEStrConsts;

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
  EnableRemoteControlOpt='--remote-control';
  DebugLogOpt='--debug-log=';
  LanguageOpt='--language=';
  LazarusDirOpt ='--lazarusdir=';

procedure ParseCommandLine(aCmdLineParams : TStrings; out IDEPid : Integer;
            out ShowSplashScreen: boolean);
function GetCommandLineParameters(aCmdLineParams : TStrings;
            isStartLazarus : Boolean = False) : String;

function IsHelpRequested (index : Integer = 1) : Boolean;
function IsVersionRequested : boolean;
function ParamIsOption(ParamIndex : integer; const Option : string) : boolean;
function ParamIsOptionPlusValue(ParamIndex : integer;
            const Option : string; out AValue : string) : boolean;

procedure SetParamOptions(var SkipAutoLoadingLastProject,
                              StartedByStartLazarus,
                              EnableRemoteControl,
                              ShowSplashScreen,
                              Setup: Boolean);

function ExtractCmdLineFilenames : TStrings;

function GetLazarusDirectory : String;

// remote control
var
  EnableRemoteControl: boolean = false;

function SetupMainIDEInstance: boolean; // false if this is a secondary instance
function GetPidFile: string;
function IsLazarusPIDRunning(aPID: int64): boolean;
function GetRemoteControlFilename: string;
procedure CleanUpPIDFile;

implementation

{$IFDEF Linux}
{$DEFINE UseProcFileSystem}
{$ENDIF}
{$IF defined(FreeBSD) and defined(VER2_5)}
{$DEFINE UseFreeBSDKernProc}
{$ENDIF}

{$IFDEF UseFreeBSDKernProc}
uses FreeBSD, BaseUnix;
{$ENDIF}

function IsLazarusPIDRunning(aPID: int64): boolean;

  function CheckProcFileSystem: boolean;
  var
    sl: TStringList;
    Filename: String;
  begin
    Result:=false;
    Filename:='/proc/'+IntToStr(aPID)+'/cmdline';
    if not FileExists(Filename) then exit;
    sl:=TStringList.Create;
    try
      try
        sl.LoadFromFile(Filename);
        if sl.Count=0 then exit;
        if Pos('lazarus',lowercase(sl[0]))<1 then exit;
        Result:=true;
      except
      end;
    finally
      sl.Free;
    end;
  end;

  {$IFDEF UseFreeBSDKernProc}
  function CheckFreeBSDKernProc: boolean;
  var
    s: string;
  begin
    Result:=(kernproc_getpath(aPID,s)<>-1)
        and (Pos('lazarus',lowercase(s))>0);
  end;
  {$ENDIF}

begin
  Result:=true;
  {$IFDEF UseFreeBSDKernProc}
  if CheckFreeBSDKernProc then exit;
  {$ENDIF}
  {$IFDEF UseProcFileSystem}
  if CheckProcFileSystem then exit;
  {$ENDIF}
  Result:=false;
end;

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

function IsHelpRequested (index : Integer = 1) : Boolean;
begin
  Result := (ParamCount>=index) and
            ((CompareText (ParamStrUTF8(index), '--help') = 0) or
             (CompareText (ParamStrUTF8(index), '-help')  = 0) or
             (CompareText (ParamStrUTF8(index), '-?')     = 0) or
             (CompareText (ParamStrUTF8(index), '-h')     = 0));
end;

function IsVersionRequested: boolean;
begin
  Result := (ParamCount=1) and
            ((CompareText (ParamStrUTF8(1), '--version') = 0) or
             (CompareText (ParamStrUTF8(1), '-v')     = 0));
end;

function ParamIsOption(ParamIndex : integer; const Option : string) : boolean;
begin
  Result:=CompareText(ParamStrUTF8(ParamIndex),Option) = 0;
end;

function ParamIsOptionPlusValue(ParamIndex : integer;
    const Option : string; out AValue : string) : boolean;
var
  p : String;
begin
 p      := ParamStrUTF8(ParamIndex);
 Result := CompareText(LeftStr(p, length(Option)), Option) = 0;
 if Result then
   AValue := copy(p, length(Option) + 1, length(p))
 else
   AValue := '';
end;

procedure SetParamOptions(var SkipAutoLoadingLastProject,
                              StartedByStartLazarus,
                              EnableRemoteControl,
                              ShowSplashScreen,
                              Setup: Boolean);
var
  i      : integer;
  AValue : String;
begin
  for i:= 1 to ParamCount do
  begin
    //DebugLn(['TMainIDE.ParseCmdLineOptions ',i,' "',ParamStrUTF8(i),'"']);
    if ParamIsOptionPlusValue(i, PrimaryConfPathOptLong, AValue) then
      SetPrimaryConfigPath(AValue)
    else if ParamIsOptionPlusValue(i, PrimaryConfPathOptShort, AValue) then
      SetPrimaryConfigPath(AValue)
    else if ParamIsOptionPlusValue(i, SecondaryConfPathOptLong, AValue) then
      SetSecondaryConfigPath(AValue)
    else if ParamIsOptionPlusValue(i, SecondaryConfPathOptShort, AValue) then
      SetSecondaryConfigPath(AValue)
    else if ParamIsOption(i, NoSplashScreenOptLong) or
        ParamIsOption(i, NoSplashScreenOptShort)    then
      ShowSplashScreen := false
    else if ParamIsOption(i, ShowSetupDialogOptLong) then
      Setup:=true
    else if ParamIsOption(i, SkipLastProjectOpt) then
      SkipAutoLoadingLastProject := true
    else if ParamIsOption(i, StartedByStartLazarusOpt) then
      StartedByStartLazarus := true
    else if ParamIsOption(i, EnableRemoteControlOpt) then
      EnableRemoteControl := true;
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

function GetPidFile: string;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+'pid.txt';
end;

function SetupMainIDEInstance: boolean;

  procedure WritePIDFile(const Filename: string; aPID: int64);
  var
    Dir: String;
    sl: TStringList;
  begin
    debugln(['WritePIDFile File="',Filename,'" PID=',aPID]);
    sl:=TStringList.Create;
    try
      sl.Add(IntToStr(aPID));
      try
        Dir:=ChompPathDelim(ExtractFilePath(Filename));
        if not DirectoryExistsUTF8(Dir) then begin
          if not CreateDirUTF8(Dir) then
            debugln(['WritePIDFile failed to create directory ',Dir]);
          exit;
        end;
        sl.SaveToFile(UTF8ToSys(Filename));
      except
        on E: Exception do begin
          debugln(['WritePIDFile "',Filename,'" failed:']);
          debugln(E.Message);
        end;
      end;
    finally
      sl.Free;
    end;
  end;

  function ReadPIDFile(const Filename: string; out ConfigPID: int64): boolean;
  var
    sl: TStringList;
  begin
    Result:=false;
    ConfigPID:=-1;
    debugln(['ReadPIDFile ',Filename]);
    if not FileExistsUTF8(Filename) then exit;
    sl:=TStringList.Create;
    try
      try
        sl.LoadFromFile(UTF8ToSys(Filename));
        ConfigPID:=StrToInt64(sl[0]);
        Result:=true;
        debugln(['ReadPIDFile ConfigPID=',ConfigPID]);
      except
        on E: Exception do begin
          debugln(['ReadPIDFile "',Filename,'" failed:']);
          debugln(E.Message);
        end;
      end;
    finally
      sl.Free;
    end;
  end;

  procedure SendCmdlineActionsToMainInstance;
  var
    sl: TStringList;
    Param: String;
    Filename: String;
    i: Integer;
  begin
    sl:=TStringList.Create;
    try
      sl.Add('Show');
      for i:=1 to Paramcount do begin
        Param:=ParamStrUTF8(i);
        if (Param='') or (Param[1]='-') then continue;
        sl.Add('Open '+Param);
      end;
      Filename:=GetRemoteControlFilename;
      try
        debugln(['SendCmdlineActionsToMainInstance Commands="',sl.Text,'"']);
        sl.SaveToFile(UTF8ToSys(Filename));
      except
        on E: Exception do begin
          debugln(['SendCmdlineActionsToMainInstance failed to write ',Filename]);
          debugln(E.Message);
        end;
      end;
    finally
      sl.Free;
    end;
  end;

var
  PIDFilename: String;
  MyPID, ConfigPID: int64;
  PIDRead: Boolean;
begin
  Result:=true;
  if not EnableRemoteControl then exit;

  // check if another IDE (of this user and same configuration) is already
  // running. Request it to handle the show and handle the command line
  // parameters (e.g. open files). And if successful return false.
  // Otherwise become the main instance.
  PIDFilename:=GetPidFile;
  MyPID:=GetProcessID;
  ConfigPID:=-1;
  PIDRead:=ReadPIDFile(PIDFilename,ConfigPID);
  if PIDRead and (ConfigPID<>MyPID) then begin
    // there is a pid file from another instance
    if not IsLazarusPIDRunning(ConfigPID) then begin
      // clean up
      DeleteFileUTF8(PIDFilename);
      PIDRead:=false;
    end;
  end;
  if not FileExistsUTF8(PIDFilename) then begin
    // try to become the main instance
    WritePIDFile(PIDFilename,MyPID);
    PIDRead:=false;
  end;
  if not PIDRead then
    PIDRead:=ReadPIDFile(PIDFilename,ConfigPID);
  if ConfigPID=MyPID then begin
    // this is the main instance
    exit;
  end;
  // this is a second instance
  Result:=false;

  SendCmdlineActionsToMainInstance;
end;

function GetRemoteControlFilename: string;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+'ideremotecontrol.txt';
end;

procedure CleanUpPIDFile;
begin
  if EnableRemoteControl then
    DeleteFileUTF8(GetRemoteControlFilename);
end;

end.

