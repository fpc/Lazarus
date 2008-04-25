unit EmptyMethods1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type

  { TSmallDirtyClass }

  TSmallDirtyClass = class(TPersistent)
  published
    procedure DoSomething;
  end;

  TDirtyClass = class(TPersistent)
  published
  private
  public
  end;

implementation

{ TSmallDirtyClass }

procedure TSmallDirtyClass.DoSomething;
begin

end;

end.

