// Add defines here. This file should be included in all LCL units headers
{$define UseCLDefault}

// For Android, otherwise the LCL will dependent on libc
{$IFDEF Android}
  {$DEFINE DisableCWString}
  {$DEFINE DisableIconv}
{$ENDIF}
