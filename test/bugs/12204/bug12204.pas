program bug12204;

var
 s1: AnsiString;
 s2: string;

begin
 s1 := 's1';
 WriteLn('s1 = ', s1, ' : ', SizeOf(s1));

 s2 := 's2';
 WriteLn('s2 = ', s2, ' : ', SizeOf(s2));
end.
