// Add defines here. This file should be included in all LCL units headers
{$define UseCLDefault}

// For Android, otherwise the LCL will dependent on libc
{$IF defined(Android)}//defined(CPUARM) and defined(LINUX)}
  {$DEFINE DisableCWString}
  {$DEFINE DisableIconv}
  // This IFNDEF Windows is for people that hard-code -dAndroid in their LCLBase package,
  // lets not enable paswstring when compiling for Windows, so we don't need
  // to keep changing defines to switch between Windows and Android
  {$IFNDEF Windows}
    {$DEFINE EnablePasWString}
  {$ENDIF}
{$ENDIF}

