unit AbstractClass1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type
  TAbstractClass = class
  public
    procedure Increase; virtual; abstract;
    procedure Decrease; virtual; abstract;
  end;
  
  TMyClass = class(TAbstractClass)
  end;

implementation

end.

