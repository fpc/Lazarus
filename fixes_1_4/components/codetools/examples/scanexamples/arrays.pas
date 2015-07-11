program arrays;

{$mode objfpc}{$H+}

type
  TFoo=record
    a:byte;
    b:word;
  end;
  TT1=array[1..5,1..5] of TFoo;
  TT2=array[1..5] of array[1..5] of TFoo;
  TT3=array[1..5,1..5] of byte;
  TT4=array[1..5] of array[1..5] of byte;

var
  t1:TT1;
  t2:TT2;
  t3:TT3;
  t4:TT4;

begin
  writeln(t1[1,1].a);//Works
  writeln(t1[1][1].);//Error: default property not found
  writeln(t2[1,1].);//Error: illegal qualifier . found
  writeln(t2[1][1].a);//Works
  writeln(t3[1,1].);//Error: illegal qualifier . found (as expected)
  writeln(t3[1][1].);//EAccessViolation: Access violation (!)
  writeln(t4[1,1].);//Error: illegal qualifier . found (as expected)
  writeln(t4[1][1].);//Error: illegal qualifier . found (as expected)
end.

