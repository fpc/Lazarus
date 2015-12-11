{
  ./testcodetools --format=plain --suite=TestFindDeclaration_TypeHelper
}
unit fdt_typehelper;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils;

type

  { TShortStringTypeHelper }

  TShortStringTypeHelper = type helper for ShortString
  public
    function MyVar: integer;
  end;

  { TStringTypeHelper }

  TStringTypeHelper = type helper for String
  public
    function Get45: integer;
  end;

  { TLongIntTypeHelper }

  TLongIntTypeHelper = type helper for LongInt
  public
    function GetStr: string;
  end;

  { TEnumTypeHelperOld }

  TEnumTypeHelperOld = type helper for TShiftStateEnum
  public
    function Old78: integer;
  end;

  { TEnumTypeHelper }

  TEnumTypeHelper = type helper(TEnumTypeHelperOld) for TShiftStateEnum
  public
    function Get78: integer;
  end;

procedure DoIt;

implementation

procedure DoIt;
var
  ShortS: ShortString;
  AnsiS: string;
  i: integer;
  e: TShiftStateEnum;
begin
  ShortS:='ABC';
  writeln('DoIt ',ShortS.MyVar{declaration:fdt_typehelper.TShortStringTypeHelper.MyVar});

  i:='Hello'.Get45{declaration:fdt_typehelper.TStringTypeHelper.Get45};
  if i<>45 then ;

  AnsiS:=i.GetStr{declaration:fdt_typehelper.TLongIntTypeHelper.GetStr};
  if AnsiS<>'' then ;

  e:=ssDouble;
  i:=e.Old78{declaration:fdt_typehelper.TEnumTypeHelperOld.Old78};
  if i<>78 then ;
  i:=e.Get78{declaration:fdt_typehelper.TEnumTypeHelper.Get78};
  if i<>78 then ;
end;

{ TEnumTypeHelperOld }

function TEnumTypeHelperOld.Old78: integer;
begin
  Result:=78;
end;

{ TEnumTypeHelper }

function TEnumTypeHelper.Get78: integer;
begin
  Result:=78;
end;

{ TLongIntTypeHelper }

function TLongIntTypeHelper.GetStr: string;
begin
  Result:=IntToStr(Self)
end;

{ TShortStringTypeHelper }

function TShortStringTypeHelper.MyVar: integer;
begin
  Result:=123;
end;

{ TStringTypeHelper }

function TStringTypeHelper.Get45: integer;
begin
  Result:=45;
end;

end.

