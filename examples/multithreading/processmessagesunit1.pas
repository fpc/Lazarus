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
    Demo to show, how to process a big file with the main thread.
}
unit ProcessMessagesUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ComCtrls, FileUtil, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ProgressBar1: TProgressBar;
    StartStopButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
  private
    procedure Run;
    procedure UpdateButton;
  public
    Running: boolean;
    Aborting: boolean;
    Filename: String;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Filename:='processmessagesunit1.pas';
end;

procedure TForm1.StartStopButtonClick(Sender: TObject);
begin
  if not Running then
    Run
  else
    Aborting:=true;
end;

procedure TForm1.Run;
var
  fs: TFileStream;
  Buffer: string;
  Count: LongInt;
  i: Integer;
  Last: TDateTime;
begin
  if Running then exit;
  Running:=true;
  UpdateButton;
  try
    // open a file
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmOpenRead);
    try
      SetLength(Buffer,10);
      while true do
      begin

        // process all user events, like clicking on the button
        Application.ProcessMessages;
        if Aborting or Application.Terminated then break;

        // read some bytes
        Count:=fs.Read(Buffer[1],length(Buffer));
        if Count=0 then break;

        // process ...
        for i:=1 to Count do
        begin
          Last:=Now;
          repeat
          until Now-Last>(double(1)/fs.Size)/10000;
        end;

        // show progress
        ProgressBar1.Position:=ProgressBar1.Min
               +((ProgressBar1.Max-ProgressBar1.Min+1)*fs.Position) div fs.Size;
      end;
    finally
      fs.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg('Error',E.Message,mtError,[mbCancel],0);
    end;
  end;
  Aborting:=false;
  Running:=false;
  UpdateButton;
end;

procedure TForm1.UpdateButton;
begin
  if Running then begin
    if Aborting then
      StartStopButton.Caption:='Aborting ...'
    else
      StartStopButton.Caption:='Running ...';
  end else begin
    StartStopButton.Caption:='Start';
  end;
end;

initialization
  {$I processmessagesunit1.lrs}

end.

