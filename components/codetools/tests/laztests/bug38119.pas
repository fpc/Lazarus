program Project1;
type
  TDoubleRec = packed record
    case byte of
      0: (Bytes : array[0..7] of Byte);
      1: (Words : array[0..3] of Word);
      2: (Data : QWord);
      3: (Value: Double);
  end;

var
  V: Double;
  B: byte absolute TDoubleRec(V).Bytes[0];
begin
end.

