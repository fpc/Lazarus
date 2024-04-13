unit IdeConfigPckTest;

(*
  The package IdeConfig can be used by command line tools (like LazBuild).
  Therefore it should not depend on the LCL or GUI related packages
*)

{$IFDEF LCL}
  {$error The package IdeConfig must not depend on the LCL}
{$ENDIF}

interface

implementation

end.

