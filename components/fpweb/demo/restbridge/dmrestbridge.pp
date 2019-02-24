unit dmRestBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldbrestbridge, sqldbrestschema, pqconnection, sqldbrestauth,
  // Register formats
  sqldbrestcsv ,sqldbrestxml, sqldbrestcds;

type

  { TRestDataModule }

  TRestDataModule = class(TDataModule)
    AuthBasic: TRestBasicAuthenticator;
    Dispatcher: TSQLDBRestDispatcher;
    ExpensesSchema: TSQLDBRestSchema;
  private

  public

  end;

var
  RestDataModule: TRestDataModule;

implementation

{$R *.lfm}

end.

