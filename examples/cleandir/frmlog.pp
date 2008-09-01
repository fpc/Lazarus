{ Directory cleaning component logging window

  Copyright (C) 2007 Michael Van Canneyt michael@freepascal.org

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

unit frmlog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  FileUtil, StdCtrls, dircleaner;

type

  { TLogForm }

  TLogForm = class(TForm)
    BClose: TButton;
    BSave: TButton;
    Label1: TLabel;
    LBLog: TListBox;
    SDlog: TSaveDialog;
    procedure BCloseClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCleaner: TCleanDirs;
    FRunning : Boolean;
    procedure SetCleaner(const AValue: TCleanDirs);
    { private declarations }
  public
    { public declarations }
    Procedure OnLog(Msg : String);
    Property Cleaner : TCleanDirs Read FCleaner Write SetCleaner;
  end; 

var
  LogForm: TLogForm;

implementation

resourcestring
  SClose = '&Close';
  
  { TLogForm }

procedure TLogForm.BSaveClick(Sender: TObject);
begin
  If SDLog.execute then
    LBLog.Items.SaveToFile(UTF8ToSys(SDLog.FileName));
end;

procedure TLogForm.BCloseClick(Sender: TObject);
begin
  if FRunning then
    FCleaner.Cancel
  else
    Close;
end;

procedure TLogForm.FormShow(Sender: TObject);
begin
  FRunning:=True;
  Try
    FCleaner.execute;
    BClose.Caption:=SClose;
  Finally
    FRunning:=False;
  end;
end;

procedure TLogForm.SetCleaner(const AValue: TCleanDirs);
begin
  if FCleaner=AValue then exit;
  FCleaner:=AValue;
  If Assigned(FCleaner) then
    FCleaner.OnLog:=@Self.OnLog;
end;

procedure TLogForm.OnLog(Msg: String);
begin
  LBLog.Items.Add(Msg);
  Application.ProcessMessages;
end;

initialization
  {$I frmlog.lrs}

end.

