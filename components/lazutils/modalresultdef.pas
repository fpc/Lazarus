unit ModalResultDef;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TModalResult = low(Integer)..high(Integer);
  PModalResult = ^TModalResult;

const
  // Used for ModalResult
  mrNone = 0;
  mrOK = mrNone + 1;
  mrCancel = mrNone + 2;
  mrAbort = mrNone + 3;
  mrRetry = mrNone + 4;
  mrIgnore = mrNone + 5;
  mrYes = mrNone + 6;
  mrNo = mrNone + 7;
  mrAll = mrNone + 8;
  mrNoToAll = mrNone + 9;
  mrYesToAll = mrNone + 10;
  mrClose = mrNone + 11;
  mrLast = mrClose;

  // String representation of ModalResult values
  ModalResultStr: array[mrNone..mrLast] of shortstring = (
    'mrNone',
    'mrOk',
    'mrCancel',
    'mrAbort',
    'mrRetry',
    'mrIgnore',
    'mrYes',
    'mrNo',
    'mrAll',
    'mrNoToAll',
    'mrYesToAll',
    'mrClose');


implementation

end.

