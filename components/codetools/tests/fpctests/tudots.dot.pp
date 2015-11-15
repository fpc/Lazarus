unit tudots.dot;

interface

var
  test: char;

procedure t;

implementation

uses
  tudots{declaration:tudots}, tudots.dot.next{ todo declaration:tudots.dot.next};

// test that type is resolved
var
  test1: tudots.dot.next.ttest{declaration:tudots.dot.next.ttest};

procedure t;
begin
  // test that we resolve the next identifier to the local variable test
  tudots.dot.test{declaration:tudots.dot.test} := 'c';
end;

end.

