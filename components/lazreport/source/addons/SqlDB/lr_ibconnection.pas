unit LR_IBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LR_Class, LR_DBComponent, sqldb,
  IBConnection, lr_SQLQuery;

type
  TLR_IBConnection = class(TComponent)
  end;

  { TIBPQConnection }

  TLRIBConnection = class(TLRSQLConnection)
  public
    constructor Create(AOwnerPage:TfrPage); override;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LazReport',[TLR_IBConnection]);
end;

var
  lrBMP_IBConnection:TBitmap = nil;

procedure InitLRComp;
begin
  if not assigned(lrBMP_IBConnection) then
  begin
    lrBMP_IBConnection := TbitMap.Create;
    lrBMP_IBConnection.LoadFromResourceName(HInstance, 'TLRIBConnection');
    frRegisterObject(TLRIBConnection, lrBMP_IBConnection, 'TLRIBConnection', nil, otlUIControl, nil);
  end;
end;

{ TLRIBConnection }

constructor TLRIBConnection.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'LRIBConnection';
  FConnection:=TIBConnection.Create(OwnerForm);
  FConnection.Transaction:=FSQLTransaction;
end;

initialization
  InitLRComp;
finalization
  if Assigned(lrBMP_IBConnection) then
    FreeAndNil(lrBMP_IBConnection);
end.

