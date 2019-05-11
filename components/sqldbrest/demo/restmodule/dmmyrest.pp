unit dmmyrest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, HTTPDefs, fpHTTP, sqldbrestmodule, sqldbrestbridge,
  sqldbrestauth, sqldbrestschema;

type

  { TMyRest }

  TMyRest = class(TSQLDBRestModule)
    AuthBasic: TRestBasicAuthenticator;
    MyDispatcher: TSQLDBRestDispatcher;
    ExpensesSchema: TSQLDBRestSchema;
  private

  public

  end;

var
  MyRest: TMyRest;

implementation

{$R *.lfm}

initialization
  TMyRest.RegisterModule('REST');
end.

