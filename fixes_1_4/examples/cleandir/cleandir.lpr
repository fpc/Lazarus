{ Directory cleaner - command-line interface

  Copyright (C) 2007 Michael Van Canneyt michael@freepascal.org

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
program cleandir;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  CustApp,
  dircleaner;

ResourceString
  SErrNoConfig      = 'No configuration file found.';
  SErrInvalidConfig = 'Configuration file "%s" does not exist.';
  SErrNoDirectories = 'No directories to clean';
  SUsage0 = 'Usage: %s [options] directory1 ... directoryN';
  SUsage1 = 'Where options is one or more of:';
  SUsageh = '-h --help         This help message';
  SUsagec = '-c --config=file  Filename for configuration data';
  SUsagev = '-v --verbose      Log more messages';
  SUsagel = '-l --logonly      Only log actions, do not execute commands';
  SUsageq = '-q --quiet        Do not log actions (disables -l)';
  SUsages = '-s --stoponerror  Stop at the first error.';
  SUsager = '-r --recurse      Recurse directories specified on command-line';
  SUsaged = '-d --standarddirs Clean standard dirs from configuration file';
  
  


Type

  { TCleanDirs }
  TRunMode = (rmHelp,rmExecute);

  TCleanDir = Class (TCustomApplication)
  private
    FCleaner: TCleanDirs;
    Function ProcessCommandLine : TRunMode;
  Public
    Constructor Create(AOwner : TComponent); override;
    Procedure DoRun; override;
    Procedure Usage;
    Procedure OnLog(Msg : String);
    Property Cleaner : TCleanDirs Read FCleaner;
  end;

{ TCleanDir }

Var
  Application : TCleanDir;

function TCleanDir.ProcessCommandLine: TRunMode;

Const
  OptCount = 8;
  Opts : Array[1..OptCount] of string =
    ('help','config','verbose','logonly','quiet',
     'stoponerror','recurse','standarddirs');
  OptChars = 'hcvlqsrd';

Var
  FN : String;
  OptL,Dirs : TStrings;
  I : integer;

begin
  Result:=rmHelp;
  If HasOption('h','help') then
    Exit;
  Dirs:=TStringList.Create;
  try
    OptL:=TStringList.Create;
    try
      For I:=1 to OptCount do
        OptL.Add(Opts[i]);
      FN:=CheckOptions(OptChars,OptL,Nil,Dirs);
    finally
      OptL.Free;
    end;
    If (FN<>'')  then
      begin
      Writeln(StdErr,FN);
      Exit;
      end;
    FN:='';
    If HasOption('c','config') then
      begin
      FN:=Self.GetOptionValue('c','config');
      If Not FileExists(FN) then
        begin
        Writeln(StdErr,format(SErrInvalidConfig,[FN]));
        Exit;
        end;
      end
    else
      begin
      FN:=GetAppConfigFile(false,false);
      If Not FileExists(FN) then
        begin
        FN:=GetAppConfigFile(True,false);
        If Not FileExists(FN) then
          FN:='';
        end;
      If (FN='') then
        begin
        Writeln(StdErr,SErrNoConfig);
        Exit;
        end;
      end;
    FCleaner.LoadFromFile(FN);
    If Not HasOption('q','quiet') then
      begin
      FCleaner.OnLog:=@Self.OnLog;
      FCleaner.LogAllFiles:=HasOption('v','verbose');
      FCleaner.LogOnly:=HasOption('l','logonly');
      end;
    If Hasoption('s','stoponerror') then
      FCleaner.StopOnError:=True;
    if Not HasOption('d','standarddirs') then
      FCleaner.Directories.Clear;
    If (Dirs.Count>0) then
      begin
      For I:=0 to Dirs.Count-1 do
        With FCleaner.Directories.AddDirectory do
          begin
          Path:=Dirs[i];
          Enabled:=True;
          Recurse:=HasOption('r','recurse');
          end;
      end;
    If (FCleaner.Directories.Count=0) then
      Writeln(StdErr,SErrNoDirectories)
    else
      Result:=rmExecute;
  Finally
    Dirs.Free;
  end;
end;

constructor TCleanDir.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCleaner:=TCleanDirs.Create(Self);
  StopOnException:=True;
end;

procedure TCleanDir.DoRun;
begin
  Case ProcessCommandLine of
    rmHelp    : Usage;
    rmExecute : FCleaner.Execute;
  end;
  Terminate;
end;

procedure TCleanDir.Usage;
begin
  Writeln(Format(SUsage0,[ExtractFileName(paramstr(0))]));
  Writeln(SUsage1);
  Writeln(SUsagec);
  Writeln(SUsaged);
  Writeln(SUsageh);
  Writeln(SUsagel);
  Writeln(SUsageq);
  Writeln(SUsager);
  Writeln(SUsages);
  Writeln(SUsagev);
end;

procedure TCleanDir.OnLog(Msg: String);
begin
  Writeln(Msg);
end;

begin
  OnGetApplicationName:=@CleanDirApp;
  Application:=TCleandir.Create(nil);
  Application.Title:='cleandisk';
  Application.Run;
  Application.Free;
end.

