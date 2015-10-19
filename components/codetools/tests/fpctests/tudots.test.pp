{ %fail }
unit tudots.test;

interface

// this must fail
var
  test: tudots.dot.next.ttest{ todo };

implementation

uses
  tudots.dot.next{ todo declaration:tudots.dot.next};

end.

