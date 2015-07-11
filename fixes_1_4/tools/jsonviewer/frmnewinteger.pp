{ JSON data viewer : collect data for new integer

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
unit frmnewinteger;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ExtCtrls, fpjson;

type

  { TNewNumberForm }

  TNewNumberForm = class(TForm)
    BPNewNumber: TButtonPanel;
    EName: TEdit;
    EValue: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    RGType: TRadioGroup;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    function GetAllowName: Boolean;
    function GetAsFloat: Double;
    function GetAsInt64: Integer;
    function GetAsInteger: Integer;
    function GetMemberName: String;
    function GetNumberType: TJSONNumberType;
    procedure SetAllowName(const AValue: Boolean);
    procedure SetMemberName(const AValue: String);
    procedure SetNumberType(const AValue: TJSONNumberType);
    { private declarations }
  public
    { public declarations }
    Property AllowName:  Boolean Read GetAllowName Write SetAllowName;
    Property MemberName : String Read GetMemberName Write SetMemberName;
    Property NumberType : TJSONNumberType Read GetNumberType Write SetNumberType;
    Property AsInteger : Integer Read GetAsInteger;
    Property AsInt64 : Integer Read GetAsInt64;
    Property AsFLoat : Double Read GetAsFloat;
  end;

var
  NewNumberForm: TNewNumberForm;

implementation

{$R *.lfm}

{ TNewNumberForm }

procedure TNewNumberForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=(ModalResult<>mrOK);
  If not CanClose then
    try
      Case NumberType of
        ntFloat : GetAsFloat;
        ntInteger : GetAsInteger;
        ntInt64 : GetAsInt64;
      end;
      CanClose:=(Not AllowName) or (EName.Text<>'');
    except
      on E : Exception do
        ShowMessage(E.Message)
    end;
end;

function TNewNumberForm.GetAllowName: Boolean;
begin
  Result:=EName.Enabled;
end;

function TNewNumberForm.GetAsFloat: Double;
begin
  Result:=StrToFloat(EValue.Text);
end;

function TNewNumberForm.GetAsInt64: Integer;
begin
  Result:=StrToInt64(EValue.Text);
end;

function TNewNumberForm.GetAsInteger: Integer;
begin
  Result:=StrToInt(EValue.Text);
end;

function TNewNumberForm.GetMemberName: String;
begin
  Result:=EName.Text;
end;

function TNewNumberForm.GetNumberType: TJSONNumberType;
begin
  Result:=TJSONNumberType(RGType.ItemIndex);
end;

procedure TNewNumberForm.SetAllowName(const AValue: Boolean);
begin
  EName.Enabled:=AValue;
end;

procedure TNewNumberForm.SetMemberName(const AValue: String);
begin
  EName.Text:=AValue;
end;

procedure TNewNumberForm.SetNumberType(const AValue: TJSONNumberType);
begin
  RGType.ItemIndex:=Ord(Avalue);
end;

end.

