unit AssignExample1;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils;

type

  { TMyPersistent }

  TMyPersistent = class(TComponent)
  private
    FMyInt: integer;
  public
    procedure CopyFrom(Src: TMyPersistent);
    property MyInt: integer read FMyInt write FMyInt;
  end;

implementation

{ TMyPersistent }

procedure TMyPersistent.CopyFrom(Src: TMyPersistent);
begin

end;

end.

