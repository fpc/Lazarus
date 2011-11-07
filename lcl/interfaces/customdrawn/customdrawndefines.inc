
// Choosing the default backend
//{$ifndef CD_OPENGL} <- for the future
{$ifdef Windows}
  {$define CD_WINDOWS}
{$endif}
{$ifdef UNIX}
  {$ifdef Darwin}
    {$define CD_Cocoa}
  {$else}
    {$define CD_X11}
  {$endif}
{$endif}


