{
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

  Abstract:
    Demo to show how 5 threads increases a counter.
    With and without critical sections.
    
    With critical sections you will always get 50000.
    Without you will see different results on each run and depending on your
    system.
}
unit CriticalSectionUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, LCLProc, LCLType, LCLIntf;

type

  { TMyThread }

  TMyThread = class(TThread)
  private
    FAFinished: boolean;
  public
    procedure Execute; override;
    property AFinished: boolean read FAFinished write FAFinished;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    CountWithoutCritSecButton: TButton;
    CountWithCritSecButton: TButton;
    Label1: TLabel;
    procedure CountWithCritSecButtonClick(Sender: TObject);
    procedure CountWithoutCritSecButtonClick(Sender: TObject);
  private
  public
    CriticalSection: TCriticalSection;
    Counter: integer;
    UseCriticalSection: boolean;
    procedure DoCounting;
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.CountWithCritSecButtonClick(Sender: TObject);
begin
  UseCriticalSection:=true;
  DoCounting;
end;

procedure TForm1.CountWithoutCritSecButtonClick(Sender: TObject);
begin
  UseCriticalSection:=false;
  DoCounting;
end;

procedure TForm1.DoCounting;
var
  i: Integer;
  Threads: array[1..5] of TMyThread;
  AllFinished: Boolean;
begin
  Counter:=0;
  
  // create the CriticalSection
  InitializeCriticalSection(CriticalSection);

  // start 5 threads
  for i:=Low(Threads) to High(Threads) do
    Threads[i]:=TMyThread.Create(false);
  // wait till all threads finished
  repeat
    AllFinished:=true;
    for i:=Low(Threads) to High(Threads) do
      if not Threads[i].AFinished then AllFinished:=false;
  until AllFinished;
  // free the threads
  for i:=Low(Threads) to High(Threads) do
    Threads[i].Free;

  // free the CriticalSection
  DeleteCriticalSection(CriticalSection);
  
  // show the Counter
  Label1.Caption:='Counter='+IntToStr(Counter);
end;

{ TMyThread }

procedure TMyThread.Execute;
var
  i: Integer;
  CurCounter: LongInt;
  j: Integer;
begin
  FAFinished:=false;
  // increment the counter many times
  // Because the other threads are doing the same, it will frequently happen,
  // that 2 (or more) threads read the same number, increment it by one and
  // write the result back, overwriting the result of the other threads.
  for i:=1 to 100000 do begin
    if Form1.UseCriticalSection then
      EnterCriticalSection(Form1.CriticalSection);
    try
      CurCounter:=Form1.Counter;
      for j:=1 to 1000 do ;
      inc(CurCounter);
      Form1.Counter:=CurCounter;
    finally
      if Form1.UseCriticalSection then
        LeaveCriticalSection(Form1.CriticalSection);
    end;
  end;
  FAFinished:=true;
end;

initialization
  {$I criticalsectionunit1.lrs}

end.

