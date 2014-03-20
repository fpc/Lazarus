unit unitw1;

{$mode objfpc}{$H+}

interface

uses classes;

type
  TClassUW1Base = class
  public
    a: integer;
  end;

  TClassUW1BaseObject = class(TObject)
  public
    a: integer;
  end;

  TClassUW1BaseComponent = class(TComponent)
  public
    a: integer;
  end;

implementation

end.
