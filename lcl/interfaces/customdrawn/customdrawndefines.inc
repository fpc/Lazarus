
{$ifdef Darwin}
{$modeswitch objectivec1}{$H+}// modeswitch seams to deactivate H+
{$endif}

// For now default to Android for arm-linux,
// because LCL-CustomDrawn is our main Android widgetset.
// Remove this when Android gets it's own target
{$ifdef UNIX}{$ifndef Darwin}{$ifdef CPUARM}
  {$define Android}
{$endif}{$endif}{$endif}


// Check if a backend which can be utilized in multiple-systems is already defined
{$if defined(CD_X11) or defined(CD_Android)}
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

// Default options for various backends
{$if defined(CD_Android)}
  {$define CD_UseNativeText}
  {$define CD_HasNativeSelectItemDialog}
{$endif}
{$if defined(CD_WINDOWS)}
  //{$define CD_UseNativeText}
{$endif}

// ==================
// Other options
// ==================

// ==================
// X11 options
// ==================
{$define CD_X11_NewNativePaint}

// ==================
// Debug options
// ==================
{.$define VerboseCDPaintProfiler}
{.$define VerboseCDDrawing}
{ $define VerboseCDBitmap}
{ $define VerboseCDForms}
{.$define VerboseCDAccessibility}
{ $define CD_Debug_TTF}

