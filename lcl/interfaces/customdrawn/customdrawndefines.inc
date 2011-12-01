
{$ifdef Darwin}
{$modeswitch objectivec1}
{$endif}

{
  vm^^.GetEnv crashes HTC Wildfire, Alcatel and the Emulator for unknown reasons,
  see: http://groups.google.com/group/android-ndk/browse_thread/thread/ba542483f062a828/ef9077617794e0f5

  This prevents using Pascal to Java calls completely in those platforms.
  To turn on a work around one can use CD_Android_DontUsePascalToJNI
  and then try to implement the missing functionality by other means.

  Not that Java to Pascal calls always work
}
{$define CD_Android_DontUsePascalToJNI}

// For now default to Android for arm-linux,
// because LCL-CustomDrawn is our main Android widgetset.
// Remove this when Android gets it's own target
{$ifdef UNIX}{$ifndef Darwin}{$ifdef CPUARM}
  {$define Android}
{$endif}{$endif}{$endif}


// Check if a backend which can be utilized in multiple-systems is already defined
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


