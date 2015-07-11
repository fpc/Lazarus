unit Directives1;

{$mode objfpc}{$H+}

interface

{$IFDEF WINCE} {$IFnDEF WinIME}   {$DEFINE WithoutWinIME}   {$ENDIF} {$ENDIF}
{$IFNDEF Windows}
  {$IFDEF WithoutWinIME}
    {$DEFINE WinIME}
    {$DEFINE WinIMEFull}
  {$ENDIF}
{$ENDIF}

uses
  Classes, SysUtils;

{$IfDef FPC}
  {$Define SkippedMacro}
  const SkippedConst = 1;
  {$IFDEF Bla}
  const BlaConst = 1;
  {$ENDIF}
{$Else}
  {$Define NotFPC}
  const UsedConst = 2;
{$EndIf}

{$IfOpt R+}
  {$Define RangeCheck}
  {$R-}
  const RangeCheckDisabledConst1 = Byte(300);
{$EndIf}

{$IF defined(de)}
  const t1 = 1;
{$ELSEIF defined(ru)}
  const t2 = 2;
{$ELSEIF defined(fr)}
  const t3 = 3;
{$ELSE}
  const t4 = 4;
{$IFEND}

const RangeCheckDisabledConst2 = Word(-1);

{$IfDef RangeCheck}
  {$R+}
{$EndIf}

implementation

end.

