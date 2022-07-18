{
   The purpose of this unit is to create native pascal classes that wrap gobjects in a comfortable and usable way.
}


unit girPascalClassWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, girObjects, girpascalwriter, girpascalwritertypes;

type

  { TGObjectClass }

  TGObjectClass = class
  private
    FParentGObjectClass: TGObjectClass;
    FgirObject: TgirClass;
    FPascalUnit: TPascalUnit;
  public
    constructor Create(AParentGObjectClass: TGObjectClass; AClass: TgirClass; APascalUnit: TPascalUnit);
    property ParentGObjectClass: TGObjectClass read FParentGObjectClass;
  end;


implementation

{ TGObjectClass }

constructor TGObjectClass.Create(AParentGObjectClass: TGObjectClass; AClass: TgirClass; APascalUnit: TPascalUnit);
begin
  FParentGObjectClass := AParentGObjectClass;
  FgirObject := AClass;
  FPascalUnit:=APascalUnit;
end;

end.

