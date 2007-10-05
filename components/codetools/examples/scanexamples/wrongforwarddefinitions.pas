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
  MyNilFunc = TMyFunc(0);// should be changed to const
  Func2 = MyNilFunc;     // should be changed to const
  Func3 = Func2;         // should be changed to const

type
  FuncType1 = function (_para2:longint; _para3:pointer; _para4:pointer):longint;cdecl;
    MPI_NULL_DELETE_FN = MPI_Delete_function(0);// should be changed to const

  function ExternalFunc1(_para1:longint; _para2:pointer):longint;cdecl;external name 'ExternalFunc1';
const
    ExternalFuncAlias1 = ExternalFunc1;// should be replaced with full declaration

implementation

end.

