// Test: search include file in unit directory before searching via include path
unit Inc_UnitBeforePath;

interface

{$ IncFileDefaultExt} // no path, no extension -> search .inc, .pp, .pas

implementation

end.
