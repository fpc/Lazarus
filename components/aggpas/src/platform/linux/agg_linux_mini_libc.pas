unit agg_linux_mini_libc;

{$mode delphi}

interface

uses
  ctypes;

const
  clib = 'c';
  CLOCKS_PER_SEC = 1000000;
type
  clock_t = clong;

function clock:clock_t;cdecl;external clib name 'clock';

implementation

end.

