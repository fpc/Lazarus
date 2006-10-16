{$ifdef linux}
{$I qt4linux.pas}
{$else}
{$ifdef windows}
{$I qt4win.pas}
{$else}
{$FATAL no qt4 unit for this OS}
{$endif}
{$endif}