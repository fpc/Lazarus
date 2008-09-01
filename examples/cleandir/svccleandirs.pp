{ Directory cleaning service - service module

  Copyright (C) 2007 Michael Van Canneyt

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit svccleandirs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, daemonapp, eventlog, dircleaner, FileUtil;

type
  { TCleanDirsThread }

  TCleanDirsThread = Class(TThread)
  Private
    FConfigFile : String;
    FLog : TEventLog;
    FCleaner : TCleanDirs;
    procedure DoClean(T: TDateTime);
    procedure DoLog(Msg: String);
    function GetNextTime(LastTime: TDateTime): TDateTime;
    procedure SetupCleaner;
  Public
    Constructor Create(AConfigFile : String; ALog : TEventLog; ATerminate : TNotifyEvent);
    Procedure Execute; override;
  end;

  { TCleanDirsDaemon }

  TCleanDirsDaemon = class(TDaemon)
    procedure CleanDirsDaemonCreate(Sender: TObject);
    procedure CleanDirsDaemonExecute(Sender: TCustomDaemon);
    procedure CleanDirsDaemonStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure CleanDirsDaemonStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    { private declarations }
    FConfigFile : String;
    FThread : TCleanDirsThread;
    FLog : TEventLog;
    procedure StartLog;
    procedure ThreadStopped(Sender: TObject);
  public
    { public declarations }
  end; 
  

var
  CleanDirsDaemon: TCleanDirsDaemon;

implementation

uses dateutils;

resourcestring
  SStartExecute = 'Start cleaning directories for schedule time: %s';
  SEndExecute = 'End of cleaning directories for schedule time: %s. Duration: %s';
  SErrCleaning = 'An error occurred when cleaning: %s';
  SNextCleanTime = 'Next cleaning run: %s';
  SRunningDailyAt = 'Running in daily mode, clean time: %s';
  SRunningHourlyAt = 'Running in hourly mode, at minute: %d';
  SErrNoConfigFile = 'No configuration file found !';

// Include windows messages for eventlog component.
{$ifdef mswindows}
{$r fclel.res}
{$endif}

procedure RegisterDaemon; 
begin
  RegisterDaemonClass(TCleanDirsDaemon)
end; 

{ TCleanDirsDaemon }

procedure TCleanDirsDaemon.StartLog;
begin
  FLog:=Self.Logger;
  If (FConfigFile='') then
    Flog.Error(SErrNoConfigFile);
end;


procedure TCleanDirsDaemon.CleanDirsDaemonCreate(Sender: TObject);
begin
  If Application.Hasoption('c','config') then
    begin
    FConfigFile:=Application.GetOptionValue('c','config');
    end
  else
    begin
    FConfigFile:=GetAppConfigFile(false,false);
    If Not FileExistsUTF8(FConfigFile) then
      begin
      FConfigFile:=GetAppConfigFile(True,false);
      If Not FileExistsUTF8(FConfigFile) then
        FConfigFile:='';
      end;
    end;
end;

procedure TCleanDirsDaemon.CleanDirsDaemonExecute(Sender: TCustomDaemon);
begin

end;

procedure TCleanDirsDaemon.ThreadStopped(Sender : TObject);

begin
  FThread:=Nil;
end;

procedure TCleanDirsDaemon.CleanDirsDaemonStart(Sender: TCustomDaemon;
  var OK: Boolean);
  
begin
  StartLog;
  { Start should return immediatly. Therefore, a thread is started if
    one is not yet running. Usually, here one opens a TCP/IP socket
    and starts listening }
  OK:=(FThread=Nil) and (FConfigFile<>'');
  If OK then
    FThread:=TCleanDirsThread.Create(FConfigFile,FLog,@ThreadStopped);
end;

procedure TCleanDirsDaemon.CleanDirsDaemonStop(Sender: TCustomDaemon;
  var OK: Boolean);
  
Var
  I : Integer;
  
begin
  If Assigned(FThread) then
    begin
    FThread.Terminate;
    I:=0;
    // Wait at most 5 seconds.
    While (FThread<>Nil) and (I<50) do
      begin
      Sleep(100);
      ReportStatus;
      end;
    // Let the thread die silently.
    If (FThread<>Nil) then
      FThread.OnTerminate:=Nil;
    end;
  OK:=FThread=Nil;
end;

{ TCleanDirsThread }

constructor TCleanDirsThread.Create(AConfigFile: String; ALog: TEventLog; ATerminate : TNotifyEvent);
begin
  FConfigFile:=AConfigFile;
  FLog:=Alog;
  FreeOnTerminate:=False;
  OnTerminate:=ATerminate;
  Inherited Create(False);
end;

procedure TCleanDirsThread.DoLog(Msg : String);

begin
  If Assigned(FLog) then
    FLog.Info(Msg);
  If Terminated then
    FCleaner.Cancel;
end;

Function TCleanDirsThread.GetNextTime (LastTime : TDateTime) : TDateTime;


  // Number of days to add till day is in allowed days.
  Function NextOKDay(D : TDateTime) : Integer;

  Const
    ScheduleDays : array [1..7] of TScheduleDay
                 = (sdMonday,sdTuesday,sdwednesday,sdThursday,
                    sdFriday,sdSaturday,sdSunday);
  
  begin
    Result:=0;
    If FCleaner.ScheduleDays<>[] then
      While Not (ScheduleDays[DayOfTheWeek(D+Result)] in FCleaner.ScheduleDays) do
        Inc(Result);
  end;

Var
  DT : TDateTime;
  h,m,s,ms : Word;

begin
  Case FCleaner.ScheduleMode of
    smDaily: begin
             Result:=Date;
             // Too late for today.
             if (Time>FCleaner.DailyAt) then
               Result:=Result+1;
             Result:=Result+NextOKDay(Result)+FCleaner.DailyAt
             end;
    smHourly : begin
               DT:=Now;
               H:=NextOKDay(DT);
               If (H<>0) then
                 Result:=Date+H+EncodeTime(0,FCleaner.HourlyAt,0,0)
               else
                 begin // Still today
                 DecodeTime(DT,H,M,S,MS);
                 Result:=Date;
                 If (M>=FCleaner.HourlyAt) then
                   begin // Next hour
                   H:=H+1;
                   If (H=24) then
                     begin
                     H:=0; // Tomorrow, check next day OK.
                     Result:=Result+1+NextOKDay(Result+1);
                     end;
                   end;
                 DT:=EncodeTime(H,FCleaner.HourlyAt,0,0);
                 Result:=Result+DT;
                 end;
               end;
  end;
  DoLog(Format(SNextCleanTime,[DateTimeToStr(Result)]));
end;

procedure TCleanDirsThread.DoClean(T : TDateTime);

Var
  S,ST : String;
  DT : TDateTime;
  
begin
  try
    S:=DateTimeToStr(T);
    DoLog(Format(SStartExecute,[S]));
    // FCleaner.Execute;
    DT:=FCleaner.EndTime-FCleaner.StartTime;
    ST:=TimeToStr(DT);
    DoLog(Format(SEndExecute,[S,ST]));
  except
    On E : Exception do
      If Assigned(FLog) then
        FLog.Error(SErrCleaning,[E.Message]);
  end;
end;

procedure TCleanDirsThread.SetupCleaner;

begin
  FCleaner.OnLog:=@Self.DoLog;
  FCleaner.LoadFromFile(UTF8ToSys(FConfigFile));
  If FCleaner.ScheduleMode=smDaily then
    DoLog(Format(SRunningDailyAt,[TimeToStr(FCleaner.DailyAt)]))
  else
    DoLog(Format(SRunningHourlyAt,[FCleaner.HourlyAt]));
end;

procedure TCleanDirsThread.Execute;

  Function GetDelta (T : TDateTime) : Int64;
  
  begin
    Result:=SecondsBetween(Now,T)*1000;
    If (Result>3000) then
      Result:=3000;
  end;

Var
  T : TDateTime;
  Delta : Int64;

begin
  FCleaner:=TCleanDirs.Create(Nil);
  try
    SetupCleaner;
    T:=Now;
    Repeat
      T:=GetNextTime(T);
      Repeat
        Delta:=GetDelta(T); // 3 seconds or less
        If Not Terminated then
          Sleep(Delta);
      Until (Now>=T) or Terminated;
      If Not Terminated then
        DoClean(T);
    Until Terminated;
  finally
    FreeAndNil(FCleaner);
  end;
end;

initialization
  {$I svccleandirs.lrs}


  RegisterDaemon; 
end.

