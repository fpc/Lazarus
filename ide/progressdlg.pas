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

  Author: Mattias Gaertner

  Abstract:
    A dialog for showing the progress of a boring calculation.
}
unit ProgressDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ComCtrls, LazarusIDEStrConsts;

type

  { TIDEProgressDialog }

  TIDEProgressDialog = class(TForm)
    AbortButton: TBitBtn;
    DescriptionLabel: TLabel;
    ProgressBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  end; 
  
var
  IDEProgressDialog: TIDEProgressDialog = nil;

function ShowProgress(const SomeText: string;
                      Step, MaxStep: integer): boolean;

implementation

{$R *.lfm}

type

  { TProgressWait }

  TProgressWait = class
  public
    StartTime: TDateTime;
    StartTimeValid: boolean;
    constructor Create;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  end;

{ TProgressWait }

constructor TProgressWait.Create;
begin
  Application.AddOnIdleHandler(@ApplicationIdle);
end;

procedure TProgressWait.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  StartTimeValid:=false;
end;
  
var
  ProgressWait: TProgressWait = nil;

function ShowProgress(const SomeText: string; Step, MaxStep: integer): boolean;
const
  Delay = 1.0/86400;
  
  procedure InitDlg;
  begin
    IDEProgressDialog.DescriptionLabel.Caption:=SomeText;
    if (Step>=0) and (MaxStep>Step) then begin
      IDEProgressDialog.ProgressBar.Visible:=true;
      IDEProgressDialog.ProgressBar.Max:=MaxStep;
      IDEProgressDialog.ProgressBar.Position:=Step;
    end else begin
      IDEProgressDialog.ProgressBar.Visible:=false;
    end;
  end;
  
begin
  if IDEProgressDialog<>nil then begin
    // there is already a TIDEProgressDialog
    InitDlg;
  end else begin
    // there is no TIDEProgressDialog yet
    // create one after 1 second
    if ProgressWait=nil then
      ProgressWait:=TProgressWait.Create;
    if ProgressWait.StartTimeValid then begin
      if Now-ProgressWait.StartTime>Delay then begin
        // one second waited
        // => create a TIDEProgressDialog and show it modal
        IDEProgressDialog:=TIDEProgressDialog.Create(Application);
        InitDlg;
        IDEProgressDialog.Show;
      end;
    end else begin
      ProgressWait.StartTime:=Now;
    end;
  end;
  Result:=true;
end;

{ TIDEProgressDialog }

procedure TIDEProgressDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisPDProgress;
  DescriptionLabel.Caption:='...';
  AbortButton.Caption:=lisPDAbort;

  Application.AddOnIdleHandler(@ApplicationIdle);
end;

procedure TIDEProgressDialog.FormDestroy(Sender: TObject);
begin
  Application.RemoveOnIdleHandler(@ApplicationIdle);
end;

procedure TIDEProgressDialog.ApplicationIdle(Sender: TObject;
  var Done: Boolean);
begin
  // IDE got idle => progress dialog is not used anymore
  if Screen.FormIndex(Self)>=0 then begin
    // let the LCL close it on end of message loop
    Close;
  end else begin
    // unused and invisible -> free it
    FreeAndNil(IDEProgressDialog);
  end;
end;

finalization
  FreeAndNil(ProgressWait);

end.

