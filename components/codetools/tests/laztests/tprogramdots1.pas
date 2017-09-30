program unitdots.tprogramdots1; // defines program namespace unitdots

{$mode objfpc}{$H+}

uses
  dot{declaration:unitdots.dot},
  unitdots{declaration:unitdots}.nsA{declaration:unitdots.nsA}
    .nsAA{declaration:unitdots.nsA.nsAA}.nsAAA{declaration:unitdots.nsA.nsAA.nsAAA};

type
  TPrgColor  = dot.tcolor{declaration:unitdots.dot.tcolor};
  TStrange   = unitdots.tprogramdots1.tprgcolor{declaration:tprgcolor};

begin
end.

