// Add defines here. This file should be included in all LCL units headers
{$define UseCLDefault}

// For Android and other ARM-devices, otherwise the LCL will dependent on libc
{$IFDEF ARM}{$IFDEF UNIX}
  {$DEFINE DisableCWString}
  {$DEFINE DisableIconv}
{$ENDIF}{$ENDIF}
