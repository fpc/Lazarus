// Add defines here. This file should be included in all LCL units headers

{$DEFINE UseCLDefault}
{$WARN SYMBOL_PLATFORM OFF}

// NOREMOVEEMPTYPROCS bites double inheritance in wslclclasses.pp
{$OPTIMIZATION NOREMOVEEMPTYPROCS}

{$IFNDEF FPC_FULLVERSION}
  {$DEFINE FPDOC}
{$ENDIF}
