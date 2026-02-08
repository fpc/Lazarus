unit test_const_array_of_records;

{$mode ObjFPC}

interface

type
  TRec = record
    One: Word;
    Two: Word;
  end;
  aTRec = array [0..1] of TRec;
const
  cRec: aTRec = ((One: 111; Two: 222),
    (One{5,9;19,14;6,15;7,18;6,19;8,21;6,22;7,23;6,24;11,32;24,32;7,34;20,34;26,35}: 1111;
    Two: 2222));
  bRec: array [0..1] of TRec =
    ((One: 111; Two: 222),
    (One: 1111; Two: 2222));
  mRec: array [0..1,0..1] of TRec =
    (((One: 111; Two: 222),
    (One: 1111; Two: 2222)),
    ((One: 111; Two: 222),
    (One: 1111; Two: 2222)));
var
  Rec: TRec = (One: 111; Two: 222);
  oRec: array of TRec;
implementation

begin
  setlength(oRec,1);
  oRec[0].One:=cRec[0].One;
  oRec[0].Two:=bRec[0].Two;
  Rec.One:=cRec[1].One;
  oRec[0].Two:=mRec[0,0].One;
end.

