{ %fail }
unit tudots.test;

interface

// this must fail
var
  test: tudots.dot.next.ttest{declaration:-};

implementation

uses
  tudots.dot.next{declaration:tudots.dot.next};

end.

