unit lclbasedowngrade;

{$mode objfpc}{$H+}

{$error Your project depends on LCLBase. Replace the dependency with LCL in the project inspector. }
{ LCLBase does not work without LCL. LCLBase works only with 0.9.31+.
  Simply use only LCL as dependency in the project inspector. This will
  work with all versions of Lazarus. }

interface

uses
  Classes, SysUtils; 

implementation

end.

