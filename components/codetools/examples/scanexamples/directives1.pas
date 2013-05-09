unit Directives1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$IfDef FPC}
  {$Define SkippedMacro}
{$Else}
  {$Define NotFPC}
{$EndIf}

{$IfOpt R+}
  {$Define RangeCheck}
  {$R-}
{$EndIf}

{$IfDef RangeCheck}
  {$R+}
{$EndIf}

implementation

end.

