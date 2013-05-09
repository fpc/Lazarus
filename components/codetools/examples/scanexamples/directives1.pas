unit Directives1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$IfDef FPC}

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

