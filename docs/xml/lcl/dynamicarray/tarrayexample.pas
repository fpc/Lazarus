program tarrayexample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  strings,
  DynamicArray;
  
type
  TArrayExampleClass = class
  private
    procedure doDestroyItem(Sender: Tobject; Col,Row: Integer;var Item: Pointer);
  end;

procedure TArrayExampleClass.doDestroyItem(Sender: Tobject; Col,Row: Integer;
  var Item: Pointer);
begin
  StrDispose(Item);
end;


var
  FCols: Tarray;
  ex: TArrayExampleClass;

begin
  FCols := TArray.Create;
  ex    := TArrayExampleClass.Create;
  FCols.OnDestroyItem := @ex.doDestroyItem;

  FCols.SetLength(8,8);
  FCols.arr[0,0] := StrNew('string1');
  FCols.arr[4,7] := StrNew('string2');
  FCols.arr[4,3] := StrNew('string3');

  writeln('0,0:' + Pchar(FCols.arr[0,0]));
  writeln('4,7:' + Pchar(FCols.arr[4,7]));

  FCols.MoveColRow(True,4,5);
  writeln('after moving column 4 to 5');
  writeln('5,7:' + Pchar(FCols.arr[5,7]));
  writeln('before exchanging row 7 and 3:');
  writeln('5,3:' + Pchar(FCols.arr[5,3]));
  writeln('5,7:' + Pchar(FCols.arr[5,7]));

  FCols.ExchangeColRow(False,7,3);
  writeln('after exchanging row 7 and 3:');
  writeln('5,3:' + Pchar(FCols.arr[5,3]));
  writeln('5,7:' + Pchar(FCols.arr[5,7]));

  FCols.DeleteColRow(true,5);
  writeln('after deleting column 5:');

  try
    writeln('5,3:' + Pchar(FCols.arr[5,3])); //this raises an exception
  except
    writeln ('An exception has taken place be because 5,3 does not exist.');
  end;

  try
    writeln('5,7:' + Pchar(FCols.arr[5,7])); //this raises an exception
  except
    writeln ('An exception has taken place be because 5,7 does not exist.');
  end;

  FCols.Clear; writeln('after clear:');
  try
    writeln('4,7:' + Pchar(FCols.arr[4,7])); //this raises an exception
  except
    writeln ('An exception has taken place be because 4,7 does not exist.');
  end;

  FCols.Destroy;
  ex.Destroy;
  readln;
end.

