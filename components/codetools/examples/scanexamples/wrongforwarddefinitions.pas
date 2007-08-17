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

type
  TMyInteger = longint;
  PMyRecord = ^TMyRecord;

implementation

end.

