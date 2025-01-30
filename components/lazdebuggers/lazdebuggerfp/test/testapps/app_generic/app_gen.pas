program app_gen;
{$mode objfpc}

uses
  unit_gen1; //, Unit2;

//type
//  TMyList = specialize TFPGList<integer>;

  procedure Test;
  //var l1: TMyList;
  //  a: Integer;
  begin
   Log1;
   specialize Log<integer>;
   //specialize XLog<integer>;
   //foo;
   //
   //l1 := TMyList.Create;
   //l1.Add(1);
   //a := l1.First;
   //a := l1.Count;

   {$I bar/inc1.inc}
   {$I foo/inc1.inc}
  end;

    begin
      Test; // TEST_BREAKPOINT=BrkMain
      Test;
    end.

// Must end before line 90, or other units must be adapted
