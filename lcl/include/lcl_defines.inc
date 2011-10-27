// Add defines here. This file should be included in all LCL units headers
{$define UseCLDefault}

// For Android, otherwise the LCL will dependent on libc
{$IF defined(Android)}//defined(CPUARM) and defined(LINUX)}
  {$DEFINE DisableCWString}
  {$DEFINE DisableIconv}
  {$DEFINE EnablePasWString}
{$ENDIF}
