
{$ifdef Darwin}
{$modeswitch objectivec1}
{$endif}

// For now default to Android for arm-linux,
// because LCL-CustomDrawn is our main Android widgetset.
// Remove this when Android gets it's own target
{$ifdef UNIX}{$ifndef Darwin}{$ifdef CPUARM}
  {$define Android}
{$endif}{$endif}{$endif}

// Check if a backend is already defined
{$if defined(CD_X11)}
{$else}
  // Choosing the default backend
  {$ifdef Windows}
    {$define CD_WINDOWS}
  {$endif}
  {$ifdef UNIX}
    {$ifdef Darwin}
      {$define CD_Cocoa}
    {$else}
      {$ifdef Android}
        {$define CD_Android}
      {$else}
        {$define CD_X11}
      {$endif}
    {$endif}
  {$endif}
{$endif}


