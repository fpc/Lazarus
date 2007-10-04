unit WrongForwardDefinitions; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type
  PMyInteger = ^TMyInteger;
  TMyArray = array[0..MaxNumber] of TMyInteger;

const
  MaxNumber = 3;

type
  TMyRecord = record
    i: TMyInteger;
    Next: PMyRecord;
  end;

var AnArray: array[0..EndValue] of char;
const EndValue = TMyInteger(1);

type
  TMyInteger = longint;
  PMyRecord = ^TMyRecord;
  TMyFunc = procedure(i: integer);
  MyNilFunc = TMyFunc(0);
  Func2 = MyNilFunc;
  Func3 = Func2;

type
  MPI_Delete_function = function (_para1:MPI_Comm; _para2:longint; _para3:pointer; _para4:pointer):longint;cdecl;
    MPI_NULL_DELETE_FN = MPI_Delete_function(0);

implementation

end.

