{ JSON data viewer : collect data for new boolean

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
unit frmNewBoolean;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls;

type

  { TNewBooleanForm }

  TNewBooleanForm = class(TForm)
    BPNewBoolean: TButtonPanel;
    CBValue: TCheckBox;
    Ename: TEdit;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    function GetAllowName: Boolean;
    function GetMemberName: String;
    function GetValue: Boolean;
    procedure SetAllowName(const AValue: Boolean);
    procedure SetMemberName(const AValue: String);
    procedure SetValue(const AValue: Boolean);
    { private declarations }
  public
    { public declarations }
    Property AllowName:  Boolean Read GetAllowName Write SetAllowName;
    Property MemberName : String Read GetMemberName Write SetMemberName;
    Property Value : Boolean Read GetValue Write SetValue;
  end;

var
  NewBooleanForm: TNewBooleanForm;

implementation

{$R *.lfm}

{ TNewBooleanForm }

procedure TNewBooleanForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=(Not AllowName) or (EName.Text<>'');
end;

function TNewBooleanForm.GetAllowName: Boolean;
begin
  Result:=EName.ENabled;
end;

function TNewBooleanForm.GetMemberName: String;
begin
  Result:=EName.Text;
end;

function TNewBooleanForm.GetValue: Boolean;
begin
  Result:=CBValue.Checked;
end;

procedure TNewBooleanForm.SetAllowName(const AValue: Boolean);
begin
  Ename.Enabled:=AValue;
end;

procedure TNewBooleanForm.SetMemberName(const AValue: String);
begin
  EName.Text:=AValue;
end;

procedure TNewBooleanForm.SetValue(const AValue: Boolean);
begin
  CBValue.Checked:=AValue;
end;

end.

