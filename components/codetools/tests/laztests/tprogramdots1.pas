program unitdots.tprogramdots1; // defines program namespace unitdots

{$mode objfpc}{$H+}

uses
  dot{declaration:unitdots.dot},
  unitdots{declaration:unitdots.tprogramdots1.unitdots}.nsA{declaration:unitdots.tprogramdots1.unitdots.nsA}
    .nsAA{declaration:unitdots.tprogramdots1.unitdots.nsA.nsAA}.nsAAA{declaration:unitdots.nsA.nsAA.nsAAA};

type
  TPrgColor  = dot.tcolor{declaration:unitdots.dot.tcolor};
  TStrange   = unitdots.tprogramdots1.tprgcolor{declaration:tprgcolor};

begin
end.

