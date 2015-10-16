unit tdefaultproperty1;

{$mode objfpc}{$H+}

interface

type
  TNamedObj = class
  public
    Name: string;
  end;


  { TBaseObject }

  TBaseObject = class
  private
    function GetObject(Index: Integer): TNamedObj;
  public
    property Objects[Index: Integer]: TNamedObj read GetObject; default;
  end;

implementation

{ TBaseObject }

function TBaseObject.GetObject(Index: Integer): TNamedObj;
var
  I: Integer;
begin
  Self.Objects{declaration:tdefaultproperty1.TBaseObject.Objects}[I].Name{declaration:tdefaultproperty1.TNamedObj.Name}:='';
  Self[I].Name{declaration:tdefaultproperty1.TNamedObj.Name}:='';
end;

end.

