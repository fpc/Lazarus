program TestProcMod;
{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}
{$ModeSwitch functionreferences}

procedure Scope1;
var
  // in var, after the ";" there is no more modifiers
  foo: procedure deprecated experimental;
  unimplemented: procedure cdecl; deprecated = nil;
  experimental: procedure;
begin end;

type

  deprecated = integer;
  cdecl = word;

  p1 = procedure(deprecated: deprecated) cdecl stdcall;
  p2 = procedure(deprecated: deprecated) cdecl; stdcall;

  proc1 = procedure(deprecated: deprecated) deprecated;
  proc2 = procedure(deprecated: deprecated) deprecated 'abc';
  proc3 = procedure(deprecated: deprecated) experimental deprecated;
  proc4 = procedure(deprecated: deprecated) experimental deprecated 'abc';
  proc5 = procedure(deprecated: deprecated) deprecated experimental;
  proc6 = procedure(deprecated: deprecated) deprecated 'abc' experimental;
  proc7 = procedure(deprecated: deprecated) deprecated; experimental;
  proc8 = procedure(deprecated: deprecated) deprecated 'abc'; experimental;

  proc1x = procedure(deprecated: deprecated); deprecated;
  proc2x = procedure(deprecated: deprecated); deprecated 'abc';
  proc3x = procedure(deprecated: deprecated); experimental deprecated;
  proc4x = procedure(deprecated: deprecated); experimental deprecated 'abc';
  proc5x = procedure(deprecated: deprecated); deprecated experimental;
  proc6x = procedure(deprecated: deprecated); deprecated 'abc' experimental;

  proc1c = procedure(deprecated: deprecated) cdecl; deprecated;
  proc2c = procedure(deprecated: deprecated) cdecl; deprecated 'abc';
  proc3c = procedure(deprecated: deprecated) cdecl; experimental deprecated;
  proc4c = procedure(deprecated: deprecated) cdecl; experimental deprecated 'abc';
  proc5c = procedure(deprecated: deprecated) cdecl; deprecated experimental;
  proc6c = procedure(deprecated: deprecated) cdecl; deprecated 'abc' experimental;

  proc1cx = procedure(deprecated: deprecated); cdecl; deprecated;
  proc2cx = procedure(deprecated: deprecated); cdecl; deprecated 'abc';
  proc3cx = procedure(deprecated: deprecated); cdecl; experimental deprecated;
  proc4cx = procedure(deprecated: deprecated); cdecl; experimental deprecated 'abc';
  proc5cx = procedure(deprecated: deprecated); cdecl; deprecated experimental;
  proc6cx = procedure(deprecated: deprecated); cdecl; deprecated 'abc' experimental;

  proc1c2 = procedure(deprecated: deprecated) deprecated; cdecl; unimplemented;
  proc1c21 = procedure(deprecated: deprecated) deprecated; cdecl; stdcall; unimplemented;
  proc1c22 = procedure(deprecated: deprecated) deprecated; cdecl stdcall; unimplemented;
  proc2c2 = procedure(deprecated: deprecated) deprecated 'abc'; cdecl; unimplemented;
  proc3c2 = procedure(deprecated: deprecated) experimental deprecated; cdecl; unimplemented;
  proc4c2 = procedure(deprecated: deprecated) experimental deprecated 'abc'; cdecl; unimplemented;
  proc5c2 = procedure(deprecated: deprecated) deprecated experimental; cdecl; unimplemented;
  proc6c2 = procedure(deprecated: deprecated) deprecated 'abc' experimental; cdecl; unimplemented;

  proc1o = procedure(deprecated: deprecated) of object deprecated;
  proc2o = procedure(deprecated: deprecated) of object deprecated 'abc';
  proc3o = procedure(deprecated: deprecated) of object experimental deprecated;
  proc4o = procedure(deprecated: deprecated) of object experimental deprecated 'abc';
  proc5o = procedure(deprecated: deprecated) of object deprecated experimental;
  proc6o = procedure(deprecated: deprecated) of object deprecated 'abc' experimental;
  proc7o = procedure(deprecated: deprecated) of object deprecated; experimental;
  proc8o = procedure(deprecated: deprecated) of object deprecated 'abc'; experimental;

  proc1ox = procedure(deprecated: deprecated) of object; deprecated;
  proc2ox = procedure(deprecated: deprecated) of object; deprecated 'abc';
  proc3ox = procedure(deprecated: deprecated) of object; experimental deprecated;
  proc4ox = procedure(deprecated: deprecated) of object; experimental deprecated 'abc';
  proc5ox = procedure(deprecated: deprecated) of object; deprecated experimental;
  proc6ox = procedure(deprecated: deprecated) of object; deprecated 'abc' experimental;

  proc1oc = procedure(deprecated: deprecated) of object cdecl; deprecated;
  proc2oc = procedure(deprecated: deprecated) of object cdecl; deprecated 'abc';
  proc3oc = procedure(deprecated: deprecated) of object cdecl; experimental deprecated;
  proc4oc = procedure(deprecated: deprecated) of object cdecl; experimental deprecated 'abc';
  proc5oc = procedure(deprecated: deprecated) of object cdecl; deprecated experimental;
  proc6oc = procedure(deprecated: deprecated) of object cdecl; deprecated 'abc' experimental;

  proc1n = procedure(deprecated: deprecated) is nested deprecated;
  proc2n = procedure(deprecated: deprecated) is nested deprecated 'abc';
  proc3n = procedure(deprecated: deprecated) is nested experimental deprecated;
  proc4n = procedure(deprecated: deprecated) is nested experimental deprecated 'abc';
  proc5n = procedure(deprecated: deprecated) is nested deprecated experimental;
  proc6n = procedure(deprecated: deprecated) is nested deprecated 'abc' experimental;

  proc1nx = procedure(deprecated: deprecated) is nested; deprecated;
  proc2nx = procedure(deprecated: deprecated) is nested; deprecated 'abc';
  proc3nx = procedure(deprecated: deprecated) is nested; experimental deprecated;
  proc4nx = procedure(deprecated: deprecated) is nested; experimental deprecated 'abc';
  proc5nx = procedure(deprecated: deprecated) is nested; deprecated experimental;
  proc6nx = procedure(deprecated: deprecated) is nested; deprecated 'abc' experimental;

  proc1nc = procedure(deprecated: deprecated) is nested cdecl; deprecated;
  proc2nc = procedure(deprecated: deprecated) is nested cdecl; deprecated 'abc';
  proc3nc = procedure(deprecated: deprecated) is nested cdecl; experimental deprecated;
  proc4nc = procedure(deprecated: deprecated) is nested cdecl; experimental deprecated 'abc';
  proc5nc = procedure(deprecated: deprecated) is nested cdecl; deprecated experimental;
  proc6nc = procedure(deprecated: deprecated) is nested cdecl; deprecated 'abc' experimental;


  func1 = function(deprecated: deprecated): deprecated deprecated;
  func2 = function(deprecated: deprecated): deprecated deprecated 'abc';
  func3 = function(deprecated: deprecated): deprecated experimental deprecated;
  func4 = function(deprecated: deprecated): deprecated experimental deprecated 'abc';
  func5 = function(deprecated: deprecated): deprecated deprecated experimental;
  func6 = function(deprecated: deprecated): deprecated deprecated 'abc' experimental;

  func1c = function(deprecated: deprecated): cdecl cdecl; deprecated;
  func2c = function(deprecated: deprecated): cdecl cdecl; deprecated 'abc';
  func3c = function(deprecated: deprecated): cdecl cdecl; experimental deprecated;
  func4c = function(deprecated: deprecated): cdecl cdecl; experimental deprecated 'abc';
  func5c = function(deprecated: deprecated): cdecl cdecl; deprecated experimental;
  func6c = function(deprecated: deprecated): cdecl cdecl; deprecated 'abc' experimental;

  func1c2 = function(deprecated: deprecated): cdecl deprecated; cdecl; unimplemented;
  func1c21 = function(deprecated: deprecated): cdecl deprecated; cdecl; stdcall; unimplemented;
  func1c22 = function(deprecated: deprecated): cdecl deprecated; cdecl stdcall; unimplemented;
  func2c2 = function(deprecated: deprecated): cdecl deprecated 'abc'; cdecl; unimplemented;
  func3c2 = function(deprecated: deprecated): cdecl experimental deprecated; cdecl; unimplemented;
  func4c2 = function(deprecated: deprecated): cdecl experimental deprecated 'abc'; cdecl; unimplemented;
  func5c2 = function(deprecated: deprecated): cdecl deprecated experimental; cdecl; unimplemented;
  func6c2 = function(deprecated: deprecated): cdecl deprecated 'abc' experimental; cdecl; unimplemented;

  func1o = function(deprecated: deprecated): cdecl of object deprecated;
  func2o = function(deprecated: deprecated): cdecl of object deprecated 'abc';
  func3o = function(deprecated: deprecated): cdecl of object experimental deprecated;
  func4o = function(deprecated: deprecated): cdecl of object experimental deprecated 'abc';
  func5o = function(deprecated: deprecated): cdecl of object deprecated experimental;
  func6o = function(deprecated: deprecated): cdecl of object deprecated 'abc' experimental;

  func1oc = function(deprecated: deprecated): deprecated of object cdecl; deprecated;
  func2oc = function(deprecated: deprecated): deprecated of object cdecl; deprecated 'abc';
  func3oc = function(deprecated: deprecated): deprecated of object cdecl; experimental deprecated;
  func4oc = function(deprecated: deprecated): deprecated of object cdecl; experimental deprecated 'abc';
  func5oc = function(deprecated: deprecated): deprecated of object cdecl; deprecated experimental;
  func6oc = function(deprecated: deprecated): deprecated of object cdecl; deprecated 'abc' experimental;

  func1n = function(deprecated: deprecated): deprecated is nested deprecated;
  func2n = function(deprecated: deprecated): deprecated is nested deprecated 'abc';
  func3n = function(deprecated: deprecated): deprecated is nested experimental deprecated;
  func4n = function(deprecated: deprecated): deprecated is nested experimental deprecated 'abc';
  func5n = function(deprecated: deprecated): deprecated is nested deprecated experimental;
  func6n = function(deprecated: deprecated): deprecated is nested deprecated 'abc' experimental;

  func1nc = function(deprecated: deprecated): cdecl is nested cdecl; deprecated;
  func2nc = function(deprecated: deprecated): cdecl is nested cdecl; deprecated 'abc';
  func3nc = function(deprecated: deprecated): cdecl is nested cdecl; experimental deprecated;
  func4nc = function(deprecated: deprecated): cdecl is nested cdecl; experimental deprecated 'abc';
  func5nc = function(deprecated: deprecated): cdecl is nested cdecl; deprecated experimental;
  func6nc = function(deprecated: deprecated): cdecl is nested cdecl; deprecated 'abc' experimental;


  // 3.3.1 ref to
  r_proc1 = reference to  procedure(deprecated: deprecated) deprecated;
  r_proc2 = reference to  procedure(deprecated: deprecated) deprecated 'abc';
  r_proc3 = reference to  procedure(deprecated: deprecated) experimental deprecated;
  r_proc4 = reference to  procedure(deprecated: deprecated) experimental deprecated 'abc';
  r_proc5 = reference to  procedure(deprecated: deprecated) deprecated experimental;
  r_proc6 = reference to  procedure(deprecated: deprecated) deprecated 'abc' experimental;
  r_proc7 = reference to  procedure(deprecated: deprecated) deprecated; experimental;
  r_proc8 = reference to  procedure(deprecated: deprecated) deprecated 'abc'; experimental;

  r_proc1x = reference to  procedure(deprecated: deprecated); deprecated;
  r_proc2x = reference to  procedure(deprecated: deprecated); deprecated 'abc';
  r_proc3x = reference to  procedure(deprecated: deprecated); experimental deprecated;
  r_proc4x = reference to  procedure(deprecated: deprecated); experimental deprecated 'abc';
  r_proc5x = reference to  procedure(deprecated: deprecated); deprecated experimental;
  r_proc6x = reference to  procedure(deprecated: deprecated); deprecated 'abc' experimental;

  r_proc1c = reference to  procedure(deprecated: deprecated) cdecl; deprecated;
  r_proc2c = reference to  procedure(deprecated: deprecated) cdecl; deprecated 'abc';
  r_proc3c = reference to  procedure(deprecated: deprecated) cdecl; experimental deprecated;
  r_proc4c = reference to  procedure(deprecated: deprecated) cdecl; experimental deprecated 'abc';
  r_proc5c = reference to  procedure(deprecated: deprecated) cdecl; deprecated experimental;
  r_proc6c = reference to  procedure(deprecated: deprecated) cdecl; deprecated 'abc' experimental;

  r_proc1cx = reference to  procedure(deprecated: deprecated); cdecl; deprecated;
  r_proc2cx = reference to  procedure(deprecated: deprecated); cdecl; deprecated 'abc';
  r_proc3cx = reference to  procedure(deprecated: deprecated); cdecl; experimental deprecated;
  r_proc4cx = reference to  procedure(deprecated: deprecated); cdecl; experimental deprecated 'abc';
  r_proc5cx = reference to  procedure(deprecated: deprecated); cdecl; deprecated experimental;
  r_proc6cx = reference to  procedure(deprecated: deprecated); cdecl; deprecated 'abc' experimental;

  r_proc1c2 = reference to  procedure(deprecated: deprecated) deprecated; cdecl; unimplemented;
  r_proc1c21 = reference to  procedure(deprecated: deprecated) deprecated; cdecl; stdcall; unimplemented;
  r_proc1c22 = reference to  procedure(deprecated: deprecated) deprecated; cdecl stdcall; unimplemented;
  r_proc2c2 = reference to  procedure(deprecated: deprecated) deprecated 'abc'; cdecl; unimplemented;
  r_proc3c2 = reference to  procedure(deprecated: deprecated) experimental deprecated; cdecl; unimplemented;
  r_proc4c2 = reference to  procedure(deprecated: deprecated) experimental deprecated 'abc'; cdecl; unimplemented;
  r_proc5c2 = reference to  procedure(deprecated: deprecated) deprecated experimental; cdecl; unimplemented;
  r_proc6c2 = reference to  procedure(deprecated: deprecated) deprecated 'abc' experimental; cdecl; unimplemented;

  r_func1 = reference to  function(deprecated: deprecated): deprecated deprecated;
  r_func2 = reference to  function(deprecated: deprecated): deprecated deprecated 'abc';
  r_func3 = reference to  function(deprecated: deprecated): deprecated experimental deprecated;
  r_func4 = reference to  function(deprecated: deprecated): deprecated experimental deprecated 'abc';
  r_func5 = reference to  function(deprecated: deprecated): deprecated deprecated experimental;
  r_func6 = reference to  function(deprecated: deprecated): deprecated deprecated 'abc' experimental;

  r_func1c = reference to  function(deprecated: deprecated): cdecl cdecl; deprecated;
  r_func2c = reference to  function(deprecated: deprecated): cdecl cdecl; deprecated 'abc';
  r_func3c = reference to  function(deprecated: deprecated): cdecl cdecl; experimental deprecated;
  r_func4c = reference to  function(deprecated: deprecated): cdecl cdecl; experimental deprecated 'abc';
  r_func5c = reference to  function(deprecated: deprecated): cdecl cdecl; deprecated experimental;
  r_func6c = reference to  function(deprecated: deprecated): cdecl cdecl; deprecated 'abc' experimental;

  r_func1c2 = reference to  function(deprecated: deprecated): cdecl deprecated; cdecl; unimplemented;
  r_func2c2 = reference to  function(deprecated: deprecated): cdecl deprecated 'abc'; cdecl; unimplemented;
  r_func3c2 = reference to  function(deprecated: deprecated): cdecl experimental deprecated; cdecl; unimplemented;
  r_func4c2 = reference to  function(deprecated: deprecated): cdecl experimental deprecated 'abc'; cdecl; unimplemented;
  r_func5c2 = reference to  function(deprecated: deprecated): cdecl deprecated experimental; cdecl; unimplemented;
  r_func6c2 = reference to  function(deprecated: deprecated): cdecl deprecated 'abc' experimental; cdecl; unimplemented;




var
  // in var, after the ";" there is no more modifiers
  foo: procedure(deprecated: deprecated) deprecated;
  unimplemented: procedure; cdecl;
  platform: procedure deprecated; stdcall;
  experimental: procedure;

  // = nil
  nproc0: procedure(deprecated: deprecated) = nil;
  nproc1: procedure(deprecated: deprecated) deprecated = nil;
  nproc2: procedure(deprecated: deprecated) deprecated 'abc' = nil;
  nproc3: procedure(deprecated: deprecated) experimental deprecated = nil;
  nproc4: procedure(deprecated: deprecated) experimental deprecated 'abc' = nil;
  nproc5: procedure(deprecated: deprecated) deprecated experimental = nil;
  nproc6: procedure(deprecated: deprecated) deprecated 'abc' experimental = nil;

  nproc0c: procedure(deprecated: deprecated) cdecl = nil;
  nproc1c: procedure(deprecated: deprecated) cdecl; deprecated = nil;
  nproc1c1: procedure(deprecated: deprecated) cdecl; stdcall; deprecated = nil;
  nproc1c2: procedure(deprecated: deprecated) cdecl stdcall; deprecated = nil;
  nproc2c: procedure(deprecated: deprecated) cdecl; deprecated 'abc' = nil;
  nproc3c: procedure(deprecated: deprecated) cdecl; experimental deprecated = nil;
  nproc4c: procedure(deprecated: deprecated) cdecl; experimental deprecated 'abc' = nil;
  nproc5c: procedure(deprecated: deprecated) cdecl; deprecated experimental = nil;
  nproc6c: procedure(deprecated: deprecated) cdecl; deprecated 'abc' experimental = nil;

  nproc0o: procedure(deprecated: deprecated) of object = nil;
  nproc1o: procedure(deprecated: deprecated) of object deprecated = nil;
  nproc2o: procedure(deprecated: deprecated) of object deprecated 'abc' = nil;
  nproc3o: procedure(deprecated: deprecated) of object experimental deprecated = nil;
  nproc4o: procedure(deprecated: deprecated) of object experimental deprecated 'abc' = nil;
  nproc5o: procedure(deprecated: deprecated) of object deprecated experimental = nil;
  nproc6o: procedure(deprecated: deprecated) of object deprecated 'abc' experimental = nil;


  nproc0oc: procedure(deprecated: deprecated) of object cdecl = nil;
  nproc1oc: procedure(deprecated: deprecated) of object cdecl; deprecated = nil;
  nproc2oc: procedure(deprecated: deprecated) of object cdecl; deprecated 'abc' = nil;
  nproc3oc: procedure(deprecated: deprecated) of object cdecl; experimental deprecated = nil;
  nproc4oc: procedure(deprecated: deprecated) of object cdecl; experimental deprecated 'abc' = nil;
  nproc5oc: procedure(deprecated: deprecated) of object cdecl; deprecated experimental = nil;
  nproc6oc: procedure(deprecated: deprecated) of object cdecl; deprecated 'abc' experimental = nil;

  nproc0n: procedure(deprecated: deprecated) is nested = nil;
  nproc1n: procedure(deprecated: deprecated) is nested deprecated = nil;
  nproc2n: procedure(deprecated: deprecated) is nested deprecated 'abc' = nil;
  nproc3n: procedure(deprecated: deprecated) is nested experimental deprecated = nil;
  nproc4n: procedure(deprecated: deprecated) is nested experimental deprecated 'abc' = nil;
  nproc5n: procedure(deprecated: deprecated) is nested deprecated experimental = nil;
  nproc6n: procedure(deprecated: deprecated) is nested deprecated 'abc' experimental = nil;

  nproc0nc: procedure(deprecated: deprecated) is nested cdecl = nil;
  nproc1nc: procedure(deprecated: deprecated) is nested cdecl; deprecated = nil;
  nproc2nc: procedure(deprecated: deprecated) is nested cdecl; deprecated 'abc' = nil;
  nproc3nc: procedure(deprecated: deprecated) is nested cdecl; experimental deprecated = nil;
  nproc4nc: procedure(deprecated: deprecated) is nested cdecl; experimental deprecated 'abc' = nil;
  nproc5nc: procedure(deprecated: deprecated) is nested cdecl; deprecated experimental = nil;
  nproc6nc: procedure(deprecated: deprecated) is nested cdecl; deprecated 'abc' experimental = nil;


  nfunc1: function(deprecated: deprecated): deprecated deprecated = nil;
  nfunc2: function(deprecated: deprecated): deprecated deprecated 'abc' = nil;
  nfunc3: function(deprecated: deprecated): deprecated experimental deprecated = nil;
  nfunc4: function(deprecated: deprecated): deprecated experimental deprecated 'abc' = nil;
  nfunc5: function(deprecated: deprecated): deprecated deprecated experimental = nil;
  nfunc6: function(deprecated: deprecated): deprecated deprecated 'abc' experimental = nil;

  nfunc1c: function(deprecated: deprecated): cdecl cdecl; deprecated = nil;
  nfunc2c: function(deprecated: deprecated): cdecl cdecl; deprecated 'abc' = nil;
  nfunc3c: function(deprecated: deprecated): cdecl cdecl; experimental deprecated = nil;
  nfunc4c: function(deprecated: deprecated): cdecl cdecl; experimental deprecated 'abc' = nil;
  nfunc5c: function(deprecated: deprecated): cdecl cdecl; deprecated experimental = nil;
  nfunc6c: function(deprecated: deprecated): cdecl cdecl; deprecated 'abc' experimental = nil;

  nfunc1o: function(deprecated: deprecated): cdecl of object deprecated = nil;
  nfunc2o: function(deprecated: deprecated): cdecl of object deprecated 'abc' = nil;
  nfunc3o: function(deprecated: deprecated): cdecl of object experimental deprecated = nil;
  nfunc4o: function(deprecated: deprecated): cdecl of object experimental deprecated 'abc' = nil;
  nfunc5o: function(deprecated: deprecated): cdecl of object deprecated experimental = nil;
  nfunc6o: function(deprecated: deprecated): cdecl of object deprecated 'abc' experimental = nil;

  nfunc1oc: function(deprecated: deprecated): deprecated of object cdecl; deprecated = nil;
  nfunc2oc: function(deprecated: deprecated): deprecated of object cdecl; deprecated 'abc' = nil;
  nfunc3oc: function(deprecated: deprecated): deprecated of object cdecl; experimental deprecated = nil;
  nfunc4oc: function(deprecated: deprecated): deprecated of object cdecl; experimental deprecated 'abc' = nil;
  nfunc5oc: function(deprecated: deprecated): deprecated of object cdecl; deprecated experimental = nil;
  nfunc6oc: function(deprecated: deprecated): deprecated of object cdecl; deprecated 'abc' experimental = nil;

  nfunc1n: function(deprecated: deprecated): deprecated is nested deprecated = nil;
  nfunc2n: function(deprecated: deprecated): deprecated is nested deprecated 'abc' = nil;
  nfunc3n: function(deprecated: deprecated): deprecated is nested experimental deprecated = nil;
  nfunc4n: function(deprecated: deprecated): deprecated is nested experimental deprecated 'abc' = nil;
  nfunc5n: function(deprecated: deprecated): deprecated is nested deprecated experimental = nil;
  nfunc6n: function(deprecated: deprecated): deprecated is nested deprecated 'abc' experimental = nil;

  nfunc1nc: function(deprecated: deprecated): cdecl is nested cdecl; deprecated = nil;
  nfunc2nc: function(deprecated: deprecated): cdecl is nested cdecl; deprecated 'abc' = nil;
  nfunc3nc: function(deprecated: deprecated): cdecl is nested cdecl; experimental deprecated = nil;
  nfunc4nc: function(deprecated: deprecated): cdecl is nested cdecl; experimental deprecated 'abc' = nil;
  nfunc5nc: function(deprecated: deprecated): cdecl is nested cdecl; deprecated experimental = nil;
  nfunc6nc: function(deprecated: deprecated): cdecl is nested cdecl; deprecated 'abc' experimental = nil;

  // 3.3.1 ref to
  nr_proc1: reference to  procedure(deprecated: deprecated) deprecated = nil;
  nr_proc2: reference to  procedure(deprecated: deprecated) deprecated 'abc' = nil;
  nr_proc3: reference to  procedure(deprecated: deprecated) experimental deprecated = nil;
  nr_proc4: reference to  procedure(deprecated: deprecated) experimental deprecated 'abc' = nil;
  nr_proc5: reference to  procedure(deprecated: deprecated) deprecated experimental = nil;
  nr_proc6: reference to  procedure(deprecated: deprecated) deprecated 'abc' experimental = nil;

  nr_proc1c: reference to  procedure(deprecated: deprecated) cdecl; deprecated = nil;
  nr_proc2c: reference to  procedure(deprecated: deprecated) cdecl; deprecated 'abc' = nil;
  nr_proc3c: reference to  procedure(deprecated: deprecated) cdecl; experimental deprecated = nil;
  nr_proc4c: reference to  procedure(deprecated: deprecated) cdecl; experimental deprecated 'abc' = nil;
  nr_proc5c: reference to  procedure(deprecated: deprecated) cdecl; deprecated experimental = nil;
  nr_proc6c: reference to  procedure(deprecated: deprecated) cdecl; deprecated 'abc' experimental = nil;

  nr_func1: reference to  function(deprecated: deprecated): deprecated deprecated = nil;
  nr_func2: reference to  function(deprecated: deprecated): deprecated deprecated 'abc' = nil;
  nr_func3: reference to  function(deprecated: deprecated): deprecated experimental deprecated = nil;
  nr_func4: reference to  function(deprecated: deprecated): deprecated experimental deprecated 'abc' = nil;
  nr_func5: reference to  function(deprecated: deprecated): deprecated deprecated experimental = nil;
  nr_func6: reference to  function(deprecated: deprecated): deprecated deprecated 'abc' experimental = nil;

  nr_func1c: reference to  function(deprecated: deprecated): cdecl cdecl; deprecated = nil;
  nr_func2c: reference to  function(deprecated: deprecated): cdecl cdecl; deprecated 'abc' = nil;
  nr_func3c: reference to  function(deprecated: deprecated): cdecl cdecl; experimental deprecated = nil;
  nr_func4c: reference to  function(deprecated: deprecated): cdecl cdecl; experimental deprecated 'abc' = nil;
  nr_func5c: reference to  function(deprecated: deprecated): cdecl cdecl; deprecated experimental = nil;
  nr_func6c: reference to  function(deprecated: deprecated): cdecl cdecl; deprecated 'abc' experimental = nil;

  // Issue 40497

  s_nproc0: procedure(deprecated: deprecated) ; = nil;
  s_nproc1: procedure(deprecated: deprecated) deprecated ; = nil;
  s_nproc2: procedure(deprecated: deprecated) deprecated 'abc' ; = nil;
  s_nproc3: procedure(deprecated: deprecated) experimental deprecated ; = nil;
  s_nproc4: procedure(deprecated: deprecated) experimental deprecated 'abc' ; = nil;
  s_nproc5: procedure(deprecated: deprecated) deprecated experimental ; = nil;
  s_nproc6: procedure(deprecated: deprecated) deprecated 'abc' experimental ; = nil;

  s_nproc0o: procedure(deprecated: deprecated) of object ; = nil;
  s_nproc1o: procedure(deprecated: deprecated) of object deprecated ; = nil;
  s_nproc2o: procedure(deprecated: deprecated) of object deprecated 'abc' ; = nil;
  s_nproc3o: procedure(deprecated: deprecated) of object experimental deprecated ; = nil;
  s_nproc4o: procedure(deprecated: deprecated) of object experimental deprecated 'abc' ; = nil;
  s_nproc5o: procedure(deprecated: deprecated) of object deprecated experimental ; = nil;
  s_nproc6o: procedure(deprecated: deprecated) of object deprecated 'abc' experimental ; = nil;

  s_nproc0n: procedure(deprecated: deprecated) is nested ; = nil;
  s_nproc1n: procedure(deprecated: deprecated) is nested deprecated ; = nil;
  s_nproc2n: procedure(deprecated: deprecated) is nested deprecated 'abc' ; = nil;
  s_nproc3n: procedure(deprecated: deprecated) is nested experimental deprecated ; = nil;
  s_nproc4n: procedure(deprecated: deprecated) is nested experimental deprecated 'abc' ; = nil;
  s_nproc5n: procedure(deprecated: deprecated) is nested deprecated experimental ; = nil;
  s_nproc6n: procedure(deprecated: deprecated) is nested deprecated 'abc' experimental ; = nil;

  s_nfunc0: function(deprecated: deprecated): deprecated ; = nil;
  s_nfunc1: function(deprecated: deprecated): deprecated deprecated ; = nil;
  s_nfunc2: function(deprecated: deprecated): deprecated deprecated 'abc' ; = nil;
  s_nfunc3: function(deprecated: deprecated): deprecated experimental deprecated ; = nil;
  s_nfunc4: function(deprecated: deprecated): deprecated experimental deprecated 'abc' ; = nil;
  s_nfunc5: function(deprecated: deprecated): deprecated deprecated experimental ; = nil;
  s_nfunc6: function(deprecated: deprecated): deprecated deprecated 'abc' experimental ; = nil;

  s_nfunc1o: function(deprecated: deprecated): cdecl of object deprecated ; = nil;
  s_nfunc2o: function(deprecated: deprecated): cdecl of object deprecated 'abc' ; = nil;
  s_nfunc3o: function(deprecated: deprecated): cdecl of object experimental deprecated ; = nil;
  s_nfunc4o: function(deprecated: deprecated): cdecl of object experimental deprecated 'abc' ; = nil;
  s_nfunc5o: function(deprecated: deprecated): cdecl of object deprecated experimental ; = nil;
  s_nfunc6o: function(deprecated: deprecated): cdecl of object deprecated 'abc' experimental ; = nil;

  s_nfunc1n: function(deprecated: deprecated): deprecated is nested deprecated ; = nil;
  s_nfunc2n: function(deprecated: deprecated): deprecated is nested deprecated 'abc' ; = nil;
  s_nfunc3n: function(deprecated: deprecated): deprecated is nested experimental deprecated ; = nil;
  s_nfunc4n: function(deprecated: deprecated): deprecated is nested experimental deprecated 'abc' ; = nil;
  s_nfunc5n: function(deprecated: deprecated): deprecated is nested deprecated experimental ; = nil;
  s_nfunc6n: function(deprecated: deprecated): deprecated is nested deprecated 'abc' experimental ; = nil;


  // 3.3.1 ref to
  s_nr_proc1: reference to  procedure(deprecated: deprecated) deprecated ; = nil;
  s_nr_proc2: reference to  procedure(deprecated: deprecated) deprecated 'abc' ; = nil;
  s_nr_proc3: reference to  procedure(deprecated: deprecated) experimental deprecated ; = nil;
  s_nr_proc4: reference to  procedure(deprecated: deprecated) experimental deprecated 'abc' ; = nil;
  s_nr_proc5: reference to  procedure(deprecated: deprecated) deprecated experimental ; = nil;
  s_nr_proc6: reference to  procedure(deprecated: deprecated) deprecated 'abc' experimental ; = nil;

  s_nr_func1: reference to  function(deprecated: deprecated): deprecated deprecated ; = nil;
  s_nr_func2: reference to  function(deprecated: deprecated): deprecated deprecated 'abc' ; = nil;
  s_nr_func3: reference to  function(deprecated: deprecated): deprecated experimental deprecated ; = nil;
  s_nr_func4: reference to  function(deprecated: deprecated): deprecated experimental deprecated 'abc' ; = nil;
  s_nr_func5: reference to  function(deprecated: deprecated): deprecated deprecated experimental ; = nil;
  s_nr_func6: reference to  function(deprecated: deprecated): deprecated deprecated 'abc' experimental ; = nil;



procedure Doproc1(deprecated: deprecated); deprecated;
  var v1: cdecl{declaration:cdecl}; v2: deprecated{declaration:Doproc1.deprecated};
begin end;
procedure Doproc2(deprecated: deprecated); deprecated 'abc';
  var v1: cdecl{declaration:cdecl};  v2: deprecated{declaration:Doproc2.deprecated};
begin end;
procedure Doproc3(deprecated: deprecated); experimental deprecated;
  var v1: cdecl{declaration:cdecl};
begin end;
procedure Doproc4(deprecated: deprecated); experimental deprecated 'abc';
  var v1: cdecl{declaration:cdecl};
begin end;
procedure Doproc5(deprecated: deprecated); deprecated experimental;
  var v1: cdecl{declaration:cdecl};
begin end;
procedure Doproc6(deprecated: deprecated); deprecated 'abc' experimental;
  var v1: cdecl{declaration:cdecl};
begin end;


procedure Doproc1c(deprecated: deprecated) cdecl; deprecated;
  var v1: cdecl{declaration:cdecl};
begin end;
procedure Doproc2c(deprecated: deprecated) cdecl; deprecated 'abc';
  var v1: cdecl{declaration:cdecl};
begin end;
procedure Doproc3c(deprecated: deprecated) cdecl; experimental deprecated;
  var v1: cdecl{declaration:cdecl};
begin end;
procedure Doproc4c(deprecated: deprecated) cdecl; experimental deprecated 'abc';
  var v1: cdecl{declaration:cdecl};
begin end;
procedure Doproc5c(deprecated: deprecated) cdecl; deprecated experimental;
  var v1: cdecl{declaration:cdecl};
begin end;
procedure Doproc6c(deprecated: deprecated) cdecl; deprecated 'abc' experimental;
  var v1: cdecl{declaration:cdecl};
begin end;




function Dofunc1(deprecated: deprecated): deprecated; deprecated;
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc2(deprecated: deprecated): deprecated; deprecated 'abc';
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc3(deprecated: deprecated): deprecated; experimental deprecated;
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc4(deprecated: deprecated): deprecated; experimental deprecated 'abc';
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc5(deprecated: deprecated): deprecated; deprecated experimental;
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc6(deprecated: deprecated): deprecated; deprecated 'abc' experimental;
  var v1: cdecl{declaration:cdecl};
begin end;


function Dofunc1c(deprecated: deprecated): cdecl cdecl; deprecated;
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc2c(deprecated: deprecated): cdecl cdecl; deprecated 'abc';
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc3c(deprecated: deprecated): cdecl cdecl; experimental deprecated;
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc4c(deprecated: deprecated): cdecl cdecl; experimental deprecated 'abc';
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc5c(deprecated: deprecated): cdecl cdecl; deprecated experimental;
  var v1: cdecl{declaration:cdecl};
begin end;
function Dofunc6c(deprecated: deprecated): cdecl{declaration:cdecl} cdecl; deprecated 'abc' experimental;
  var v1: cdecl{declaration:cdecl};
begin end;


begin

end.

