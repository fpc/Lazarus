{ JSON data viewer : collect data for new string

  Copyright (C) 2010 Michael Van Canneyt michael@freepascal.org

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
unit frmnewstring;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type

  { TNewStringForm }

  TNewStringForm = class(TForm)
    BPNewString: TButtonPanel;
    EName: TEdit;
    EValue: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    function GetAllowName: Boolean;
    function GetMemberName: String;
    function GetValue: String;
    procedure SetAllowName(const AValue: Boolean);
    procedure SetMemberName(const AValue: String);
    procedure SetValue(const AValue: String);
    { private declarations }
  public
    { public declarations }
    Property AllowName:  Boolean Read GetAllowName Write SetAllowName;
    Property MemberName : String Read GetMemberName Write SetMemberName;
    Property Value : String Read GetValue Write SetValue;
  end;

var
  NewStringForm: TNewStringForm;

implementation

{$R *.lfm}

{ TNewStringForm }

procedure TNewStringForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=(Not AllowName) or (EName.Text<>'');
end;

function TNewStringForm.GetAllowName: Boolean;
begin
  Result:=EName.Enabled;
end;

function TNewStringForm.GetMemberName: String;
begin
  Result:=EName.Text;
end;

function TNewStringForm.GetValue: String;
begin
  Result:=EValue.Text;
end;

procedure TNewStringForm.SetAllowName(const AValue: Boolean);
begin
  EName.Enabled:=AValue;
end;

procedure TNewStringForm.SetMemberName(const AValue: String);
begin
  EName.Text:=Avalue;
end;

procedure TNewStringForm.SetValue(const AValue: String);
begin
  EValue.Text:=Avalue;
end;

end.

