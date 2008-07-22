{
                           udlgPrintersJobs.pp
                                ------------
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Author : Olivier Guilbaud (OG)

 Abstract:
   Printer jobs dialog.
   This dialog provide an user interface for view or modify an printer
   job

 Credit
   few images : http://jimmac.musichall.cz/
   
 history
   fev 27 2004 OG - Creation
   mar 05 2004 OG - Test and design
   mar 08 2004 OG - Hold, Restart or cancel job
   mar 16 2004 OG - Delete ShowMessage() of debug
   Oct  06 2004 OG - Modified for use new Printers.pas
}
unit udlgPrintersJobs;

{$mode objfpc}{$H+}

{$DEFINE DYNLINK}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Printers,cupsPrinters,Buttons,ComCtrls,StdCtrls,
  {$IFNDEF DYNLINK}
  Cups, cups_ipp,
  {$ELSE}
  CUPSDyn,
  {$ENDIF}
  {$IFDEF darwin}
  miniCupsLibc,
  {$ELSE}
  Libc,
  {$ENDIF}
  ;

Const
  cStateJob : Array[IPP_JOB_PENDING..IPP_JOB_COMPLETED] of string =('Pending...',
                                                                    'Held',
                                                                    'Processing...',
                                                                    'Stopped',
                                                                    'Canceled',
                                                                    'Aborded',
                                                                    'Completed');
type
  TdlgPrintersJobs = class(TForm)
    cbPrinters: TComboBox;
    lstImg: TImageList;
    lvJobs: TListView;
    Panel1: TPanel;
    btnMyJobOnly: TSpeedButton;
    btnJobCompleted: TSpeedButton;
    btnRefesh: TSpeedButton;
    btnStopJob: TSpeedButton;
    btnRestartJob: TSpeedButton;
    btnDeleteJob: TSpeedButton;
    procedure btnDeleteJobClick(Sender: TObject);
    procedure btnRefeshClick(Sender: TObject);
    procedure btnRestartJobClick(Sender: TObject);
    procedure btnStopJobClick(Sender: TObject);
  private
    { private declarations }
    function TimeToDatetimeStr(atime : time_t) : string;
  public
    { public declarations }
    constructor Create(aOwner : TComponent); override;
  end; 

var
  dlgPrintersJobs: TdlgPrintersJobs;

implementation

{ TdlgPrintersJobs }
constructor TdlgPrintersJobs.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  cbPrinters.Items.Add('All');
  cbPrinters.Items.AddStrings(Printer.Printers);
  cbPrinters.ItemIndex:=0;
  cbPrinters.OnChange:=@btnRefeshClick;
  
  lstImg.AddLazarusResource('pending');
  lstImg.AddLazarusResource('helded');
  lstImg.AddLazarusResource('process');
end;


//Refresh all jobs for defined printers
procedure TdlgPrintersJobs.btnRefeshClick(Sender: TObject);
Var Jobs         : Pcups_job_t;
    Job          : Pcups_job_t;
    NumJobs,i    : Integer;
    k,aId        : longint;
    JobCompleted : Byte;
    myJobOnly    : Byte;
    C            : TListColumn;
    Li,Ls        : TListItem;
    aPrinterName : PChar;
begin
  Jobs:=nil;
  
  if Sender=btnJobCompleted then
    btnJobCompleted.Tag:=Byte(not btnJobCompleted.Down);
  if Sender=btnMyJobOnly then
    btnMyJobOnly.Tag:=Byte(not btnMyJobOnly.Tag);
    
  //List only or nor the job of current user
  myJobOnly:=btnMyJobOnly.Tag;

  //Job all job completed Yes/No
  JobCompleted:=btnJobCompleted.Tag;

  //Printer name
  aPrinterName:=nil; //All defined printers
  if cbPrinters.ItemIndex>0 then
    aPrinterName:=PChar(cbPrinters.Text);

  //Save the selected job if exists (must be before cupsGetJobs())
  if Assigned(lvJobs.selected) then
    aId:=Pcups_job_t(lvJobs.Selected.Data)^.Id
  else
    aId:=-1;

  //Enums all jobs
  NumJobs:=cupsGetJobs(@Jobs,aPrinterName,myJobOnly,JobCompleted);
  
  LvJobs.BeginUpdate;
  try
    lvJobs.Items.Clear;
    Ls:=nil;
    
    if Assigned(Jobs) then
    begin
      //Datas rows
      for i:=0 to NumJobs-1 do
      begin
        Job:=@Jobs[i];
        DateSeparator:='/';
        Li:=LvJobs.Items.Add;
        Li.Caption:=IntToStr(Job^.id);
        Li.ImageIndex:=Ord(Job^.state)-Ord(IPP_JOB_PENDING);
        Li.Data:=Job;
        if not Assigned(Ls) or (Job^.id=aId) then
          Ls:=Li;

        Li.SubItems.Add(Job^.dest);
        Li.SubItems.Add(Job^.Title);
        Li.SubItems.Add(Job^.User);
        Li.SubItems.Add(Job^.Format);
        Li.SubItems.Add(cStateJob[Job^.state]);
        Li.SubItems.Add(IntToStr(Job^.Size));
        Li.SubItems.Add(IntToStr(Job^.priority));
        Li.SubItems.Add(TimeToDatetimeStr(Job^.creation_time));
        Li.SubItems.Add(TimeToDatetimeStr(Job^.processing_time));
        Li.SubItems.Add(TimeToDatetimeStr(Job^.completed_time));
      end;
      cupsFreeJobs(NumJobs,Jobs);
    end;
  finally
    LvJobs.EndUpdate;
    LvJobs.Selected:=Ls;
  end;
end;

//Restart an holded job
procedure TdlgPrintersJobs.btnRestartJobClick(Sender: TObject);
Var Job  : Pcups_job_t;
begin
  if (lvJobs.Items.Count>0) and Assigned(lvJobs.Selected) then
  begin
    Job:=Pcups_job_t(lvJobs.Selected.Data);
    if (Job^.state=IPP_JOB_HELD) then
      TCUPSPrinter(Printer).SetJobState(Job^.id,IPP_RELEASE_JOB);
    btnRefeshClick(nil);
  end;
end;

//Stop an active job
procedure TdlgPrintersJobs.btnStopJobClick(Sender: TObject);
Var Job  : Pcups_job_t;
begin
  if (lvJobs.Items.Count>0) and Assigned(lvJobs.Selected) then
  begin
    Job:=Pcups_job_t(lvJobs.Selected.Data);
    if (Job^.state<IPP_JOB_STOPPED) and (Job^.state<>IPP_JOB_HELD) then
      TCUPSPrinter(Printer).SetJobState(Job^.id,IPP_HOLD_JOB);
    btnRefeshClick(nil);
  end;
end;

//Delete the selected job
procedure TdlgPrintersJobs.btnDeleteJobClick(Sender: TObject);
Var Job  : Pcups_job_t;
begin
  if (lvJobs.Items.Count>0) and Assigned(lvJobs.Selected) then
  begin
    Job:=Pcups_job_t(lvJobs.Selected.Data);
    cupsCancelJob(Job^.dest,Job^.id);
    btnRefeshClick(nil);
  end;
end;

//Return an String date and time with an GMT Time
function TdlgPrintersJobs.TimeToDatetimeStr(atime: time_t): string;
var lTime : PTm;
    Dt    : TDateTime;
begin
  lTime:=LocalTime(aTime);
  Result:='';
  if Assigned(lTime) then
  begin
    Try
      if lTime^.tm_year=70 then Exit;
      Dt:=EncodeDate(lTime^.tm_year+1900,lTime^.tm_mon+1,lTime^.tm_mday)+
          EncodeTime(lTime^.tm_hour,lTime^.tm_min,lTime^.tm_sec,0);
      Result:=DateTimeToStr(Dt);
    Except
    end;
  end;
end;

initialization
  {$I udlgprintersjobs.lrs}
  {$I jobsimglist.lrs}
end.
