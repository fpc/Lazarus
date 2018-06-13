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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Mattias Gaertner

  This unit manages the command line parameters for lazarus and startlazarus,
  but not lazbuild.

 ToDo:
   Linux: try pidof
}
unit IDEGuiCmdLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  // LazUtils
  LazUTF8Classes, LazUtilities, LazFileUtils,
  // Codetools
  FileProcs,
  // IDE
  LazConf, IDECmdLine;

procedure ParseGuiCmdLineParams(var SkipAutoLoadingLastProject,
                                    StartedByStartLazarus,
                                    EnableRemoteControl,
                                    ShowSplashScreen,
                                    Setup: Boolean);

// remote control
const
  EnableRemoteControlOpt='--remote-control';
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
uses FreeBSD, BaseUnix;
{$ENDIF}
{$IFDEF LCLCarbon}
{$DEFINE UseCarbonProc}
uses MacOSAll, CarbonProc;
{$ENDIF}

function IsLazarusPIDRunning(aPID: int64): boolean;

  {$IFDEF UseProcFileSystem}
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
  {$ENDIF}

  {$IFDEF UseFreeBSDKernProc}
  function CheckFreeBSDKernProc: boolean;
  var
    s: string;
  begin
    Result:=(kernproc_getpath(aPID,s)<>-1)
        and (Pos('lazarus',lowercase(s))>0);
  end;
  {$ENDIF}

  {$IFDEF UseCarbonProc}
  function CheckCarbonProc: boolean;
  var
    psn: ProcessSerialNumber;
    info: ProcessInfoRec;
    processName: CFStringRef;
    s: String;
  begin
    Result:=false;
    if GetProcessForPID(aPid,psn{%H-})<>noErr then exit;
    FillByte(info{%H-},SizeOf(info),0);
    if GetProcessInformation(psn,info)<>noErr then exit;
    processName := nil;
    if CopyProcessName(psn, processName)<>noErr then exit;
    if processName<>nil then begin
      s:=CFStringToStr(processName);
      CFRelease(processName);
      Result:=Pos('lazarus',lowercase(s))>0;
    end;
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
  {$IFDEF UseCarbonProc}
  if CheckCarbonProc then exit;
  {$ENDIF}
  Result:=false;
end;

function GetPidFile: string;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+'pid.txt';
end;

procedure ParseGuiCmdLineParams(var SkipAutoLoadingLastProject,
  StartedByStartLazarus, EnableRemoteControl, ShowSplashScreen, Setup: Boolean);
var
  i: Integer;
begin
  ParseNoGuiCmdLineParams;
  for i:= 1 to ParamsAndCfgCount do
  begin
    //DebugLn(['ParseGuiCmdLineParams ',i,' "',ParamsAndCfgStr(i),'"']);
    if ParamIsOption(i, NoSplashScreenOptLong) or
        ParamIsOption(i, NoSplashScreenOptShort)    then
      ShowSplashScreen := false
    else if ParamIsOption(i, ShowSetupDialogOptLong) then
      Setup:=true
    else if ParamIsOption(i, SkipLastProjectOpt) then
      SkipAutoLoadingLastProject := true
    else if ParamIsOption(i, StartedByStartLazarusOpt) then
      StartedByStartLazarus := true
    else if ParamIsOption(i, EnableRemoteControlOpt) then
      EnableRemoteControl := true
    else if ParamIsOption(i, '--verbose') then
      ConsoleVerbosity:=Max(1,ConsoleVerbosity+1)
    else if ParamIsOption(i, '--quiet') then
      ConsoleVerbosity:=Min(0,ConsoleVerbosity-1);
  end;
  if ConsoleVerbosity>=0 then
    CTConsoleVerbosity:=1;
end;

function SetupMainIDEInstance: boolean;

  procedure WritePIDFile(const Filename: string; aPID: int64);
  var
    Dir: String;
    sl: TStringListUTF8;
  begin
    debugln(['WritePIDFile File="',Filename,'" PID=',aPID]);
    sl:=TStringListUTF8.Create;
    try
      sl.Add(IntToStr(aPID));
      try
        Dir:=ChompPathDelim(ExtractFilePath(Filename));
        if not DirectoryExistsUTF8(Dir) then begin
          if not CreateDirUTF8(Dir) then
            debugln(['WritePIDFile failed to create directory ',Dir]);
          exit;
        end;
        sl.SaveToFile(Filename);
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
    sl: TStringListUTF8;
  begin
    Result:=false;
    ConfigPID:=-1;
    debugln(['ReadPIDFile ',Filename]);
    if not FileExistsUTF8(Filename) then exit;
    sl:=TStringListUTF8.Create;
    try
      try
        sl.LoadFromFile(Filename);
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
    sl: TStringListUTF8;
    Param: String;
    Filename: String;
    i: Integer;
  begin
    sl:=TStringListUTF8.Create;
    try
      sl.Add('Show');
      for i:=1 to ParamsAndCfgCount do begin
        Param:=ParamsAndCfgStr(i);
        if (Param='') or (Param[1]='-') then continue;
        sl.Add('Open '+Param);
      end;
      Filename:=GetRemoteControlFilename;
      try
        debugln(['SendCmdlineActionsToMainInstance Commands="',sl.Text,'"']);
        sl.SaveToFile(Filename);
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

