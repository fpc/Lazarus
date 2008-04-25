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

  { TDirtyClass }

  TDirtyClass = class(TPersistent)
  published
    procedure APublishedMethod;
  private
    procedure APrivateMethod;
  protected
    procedure AProtectedMethod;
  public
    procedure APublicMethod;
  end;

implementation

{ TSmallDirtyClass }

procedure TSmallDirtyClass.DoSomething;
begin

end;

{ TDirtyClass }

procedure TDirtyClass.APublishedMethod;
begin

end;

procedure TDirtyClass.APrivateMethod;
begin

end;

procedure TDirtyClass.AProtectedMethod;
begin

end;

procedure TDirtyClass.APublicMethod;
begin

end;

end.

