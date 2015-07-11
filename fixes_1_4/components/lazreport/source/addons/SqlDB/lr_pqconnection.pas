unit LR_PQConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LR_Class, LR_DBComponent, sqldb,
  pqconnection, lr_SQLQuery;

type
  TLR_PQConnection = class(TComponent)
  end;

  { TLRPQConnection }

  TLRPQConnection = class(TLRSQLConnection)
  public
    constructor Create(AOwnerPage:TfrPage); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LazReport',[TLR_PQConnection]);
end;

var
  lrBMP_PQConnection:TBitmap = nil;

procedure InitLRComp;
begin
  if not assigned(lrBMP_PQConnection) then
  begin
    lrBMP_PQConnection := TbitMap.Create;
    lrBMP_PQConnection.LoadFromResourceName(HInstance, 'TLRPQConnection');
    frRegisterObject(TLRPQConnection, lrBMP_PQConnection, 'TLRPQConnection', nil, otlUIControl, nil);
  end;
end;

{ TLRPQConnection }
constructor TLRPQConnection.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'LRPQConnection';
  FConnection:=TPQConnection.Create(OwnerForm);
  FConnection.Transaction:=FSQLTransaction;
end;


initialization
  InitLRComp;
finalization
  if Assigned(lrBMP_PQConnection) then
    FreeAndNil(lrBMP_PQConnection);
end.

