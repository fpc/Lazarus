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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit frmSQLConnect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, ButtonPanel, lazdatadeskstr;

type
  TTestConnectionEvent = Procedure (Sender : TObject;Const ADriver : String; Params : TStrings) of object;

  { TSQLConnectionForm }

  TSQLConnectionForm = class(TForm)
    BTest: TBitBtn;
    BPButtons: TButtonPanel;
    ECharset: TEdit;
    EHostName: TEdit;
    EDatabaseName: TEdit;
    EUserName: TEdit;
    EPassword: TEdit;
    LCharset: TLabel;
    LEUserName: TLabel;
    LEPassword: TLabel;
    LEHostName: TLabel;
    LEDatabaseName: TLabel;
    procedure BTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDriver : String;
    FOnTest: TTestConnectionEvent;
    function GetShowHost: Boolean;
    function GetString(Index: integer): String;
    procedure SetOnTest(AValue: TTestConnectionEvent);
    procedure SetShowHost(const AValue: Boolean);
    procedure SetString(Index: integer; const AValue: String);
    procedure TestConnection;
    { private declarations }
  public
    { public declarations }
    Property ShowHost : Boolean Read GetShowHost Write SetShowHost;
    Property HostName : String Index 0 Read GetString Write SetString;
    Property DatabaseName : String Index 1 Read GetString Write SetString;
    Property UserName : String Index 2 Read GetString Write SetString;
    Property Password : String Index 3 Read GetString Write SetString;
    Property Charset : String Index 4 Read GetString Write SetString;
    Property Driver : String Index 5 Read GetString Write SetString;
    Property OnTestConnection : TTestConnectionEvent Read FOnTest Write SetOnTest;
  end; 

var
  SQLConnectionForm: TSQLConnectionForm;

function GetSQLDBConnectString(HostSupported : Boolean; Initial,ADriver : String; OnTest : TTestConnectionEvent = Nil): String;

implementation

{$R *.lfm}

uses fpdatadict, fpddsqldb ,strutils;

function GetSQLDBConnectString(HostSupported : Boolean; Initial,ADriver : String; OnTest : TTestConnectionEvent = Nil): String;

Var
  L: TStringList;

begin
  Result:='';
  With TSQLConnectionForm.Create(Application) do
    try
      ShowHost:=HostSupported;
      L:=TStringList.Create;
      try
        if (Initial<>'') then
          begin
          L.CommaText:=Initial;
          if HostSupported then
            HostName:=L.Values[KeyHostName];
          DatabaseName:=L.Values[KeyDatabaseName];
          UserName:=L.Values[KeyUserName];
          Password:=XorDecode(KeyEncode,L.Values[KeyPassword]);
          Charset:=L.Values[KeyCharset];
          end;
        Driver:=ADriver;
        OnTestConnection:=OnTest;
        if (ShowModal=mrOK) then
          begin
          L.Clear;
          if HostSupported then
            L.Values[KeyHostName]:=HostName;
          L.Values[KeyDatabaseName]:=DatabaseName;
          L.Values[KeyUserName]:=UserName;
          L.Values[KeyPassword]:=XorEncode(KeyEncode,Password);
          L.Values[KeyCharset]:=Charset;
          Result:=L.CommaText;
          end;
      finally
        L.Free;
      end;
    finally
      Free;
    end;
end;

procedure TSQLConnectionForm.FormCreate(Sender: TObject);
begin
  //
  Caption:= Format(sld_Connecttoadatabase,[sld_UnknownType]);
  LEHostName.Caption:= sld_Host;
  LEDatabaseName.Caption:= sld_Database;
  LEUserName.Caption:= sld_Username;
  LEPassword.Caption:= sld_Password;
  LCharset.Caption:= sld_Charset;
  BTest.Caption:=sld_TestConnection;
  //
end;

procedure TSQLConnectionForm.TestConnection;

Var
  P : TStrings;

begin
  if Not Assigned(FOnTest) then
    exit;
  P:=TStringList.Create;
  try
    if ShowHost then
      P.Values[KeyHostName]:=HostName;
    P.Values[KeyDatabaseName]:=DatabaseName;
    P.Values[KeyUserName]:=UserName;
    P.Values[KeyPassword]:=XorEncode(KeyEncode,Password);
    P.Values[KeyCharset]:=CharSet;
    FOnTest(Self,Driver,P);
    // No errors.
    ShowMessage(sld_SuccesConnecting);
  finally
    P.Free;
  end;
end;

procedure TSQLConnectionForm.BTestClick(Sender: TObject);

begin
  TestConnection;
end;

function TSQLConnectionForm.GetShowHost: Boolean;
begin
  Result:=EHostName.Enabled;
end;

function TSQLConnectionForm.GetString(Index: integer): String;
begin
  Case Index of
    0 : Result:=EHostName.Text;
    1 : Result:=EDatabaseName.Text;
    2 : Result:=EUserName.Text;
    3 : Result:=EPassword.Text;
    4 : Result:=ECharSet.Text;
    5 : Result:=FDriver;
  end;
end;

procedure TSQLConnectionForm.SetOnTest(AValue: TTestConnectionEvent);
begin
  if FOnTest=AValue then Exit;
  FOnTest:=AValue;
  BTest.Enabled:=AValue<>Nil;
end;

procedure TSQLConnectionForm.SetShowHost(const AValue: Boolean);
begin
  EHostName.Enabled:=AValue;
end;

procedure TSQLConnectionForm.SetString(Index: integer; const AValue: String);


var
  ADesc, AType: String;
  ACap: TFPDDEngineCapabilities;
begin
  Case Index of
    0 : EHostName.Text:=AValue;
    1 : EDatabaseName.Text:=AValue;
    2 : EUserName.Text:=AValue;
    3 : EPassword.Text:=AValue;
    4 : ECharset.Text:=Avalue;
    5 :
      begin
      FDriver:=AValue;
      if GetDictionaryEngineInfo(AValue,ADesc,AType,ACap) then
        Caption:= Format(sld_Connecttoadatabase,[ADesc]);
      end;
  end;
end;

end.

