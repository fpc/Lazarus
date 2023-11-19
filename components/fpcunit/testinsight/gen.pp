{$mode objfpc}
{$h+}
uses svrtestinsight;

begin
  With TTestInsightResult.Create do
    try
      TestResult:=rtPassed;
      TestName:='Suite1.Test1';
      writeln(ToJSON);
    Finally
      free;
   end;     
end.
