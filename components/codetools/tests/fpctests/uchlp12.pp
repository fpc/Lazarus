unit uchlp12;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

type
{$M+}
  TTest = class
  private
    function GetTest6: Integer;
  strict private
    Test1: Integer;
  private
    Test2: Integer;
  strict protected
    Test3: Integer;
  protected
    Test4: Integer;
  public
    Test5: Integer;
  published
    property Test6: Integer read GetTest6;
  end;
{$M-}

implementation

function TTest.GetTest6: Integer;
begin
  Result := 0;
end;

end.
