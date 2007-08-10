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
    Demo to show, how a Thread waits for another.
}
unit WaitForUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, LCLProc;

type

  { TBaseThread }

  TBaseThread = class(TThread)
  public
    procedure Log(const Msg: string; AppendLineEnd: boolean = true);
  end;

  { TThreadA }

  TThreadA = class(TBaseThread)
  public
    WaitForB: PRtlEvent;
    procedure Execute; override;
  end;

  { TThreadB }

  TThreadB = class(TBaseThread)
  private
    FCounter: integer;
  public
    procedure Execute; override;
    property Counter: integer read FCounter write FCounter;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ACriticalSection: TRTLCriticalSection;
    MsgText: string;
    procedure AddMessage;
  public
    ThreadA: TThreadA;
    ThreadB: TThreadB;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if ThreadA=nil then
    ThreadA:=TThreadA.Create(false);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitCriticalSection(ACriticalSection);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DoneCriticalsection(ACriticalSection);
end;

procedure TForm1.AddMessage;
begin
  Memo1.Lines.Text:=MsgText;
end;

{ TThreadA }

procedure TThreadA.Execute;
begin
  Form1.ThreadB:=TThreadB.Create(false);
  // create event
  WaitForB:=RTLEventCreate;
  while not Application.Terminated do begin
    Log('A: wait for B ...');
    // wait infinitely (until B wakes A)
    RtlEventWaitFor(WaitForB);
    Log('A: ThreadB.Counter='+IntToStr(Form1.ThreadB.Counter));
  end;
end;

{ TThreadB }

procedure TThreadB.Execute;
var
  i: Integer;
begin
  Counter:=0;
  while not Application.Terminated do begin
    Log('B: Working ...');
    for i:=1 to 5 do begin
      Sleep(300);
      Log('.....  ',false);
    end;
    Log('');
    Counter := Counter + 1;
    Log('B: Wake A');
    // wake A
    RtlEventSetEvent(Form1.ThreadA.WaitForB);
  end;
end;

{ TBaseThread }

procedure TBaseThread.Log(const Msg: string; AppendLineEnd: boolean);
var
  s: String;
begin
  EnterCriticalsection(Form1.ACriticalSection);
  s:=Msg;
  if AppendLineEnd then
    s:=s+LineEnding;
  dbgout(s);
  Form1.MsgText:=Form1.MsgText+s;
  Synchronize(@Form1.AddMessage);
  LeaveCriticalsection(Form1.ACriticalSection);
end;

initialization
  {$I waitforunit1.lrs}

end.

