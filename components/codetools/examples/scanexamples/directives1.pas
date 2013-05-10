unit Directives1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$IfDef FPC}
  {$Define SkippedMacro}
  const SkippedConst = 1;
{$Else}
  {$Define NotFPC}
  const UsedConst = 2;
{$EndIf}

{$IfOpt R+}
  {$Define RangeCheck}
  {$R-}
  const RangeCheckDisabledConst1 = Byte(300);
{$EndIf}

const RangeCheckDisabledConst2 = Word(-1);

{$IfDef RangeCheck}
  {$R+}
{$EndIf}

implementation

end.

