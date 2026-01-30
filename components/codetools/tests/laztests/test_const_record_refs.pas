unit test_const_record_refs;

{$mode ObjFPC}

interface
type
  TRec = record
    One: Word;
    Two: Word;
  end;

const
  cRec: TRec = (One{findrefs:5,8;17,13;17,16;8,20;10,21}: 1; Two: 2);

var
  vRec: TRec = (One: 11; Two: 22);

implementation
begin
  vRec.One:=
    cRec.One;
  vRec.Two:=
    cRec.Two;
end.

