{$ifdef darwin}
{$I qt4mac.pas}
{$else}
{$ifdef Unix}
{$I qt4x11.pas}
{$else}
{$ifdef windows}
{$I qt4win.pas}
{$else}
{$FATAL no qt4 unit for this OS}
{$endif}
{$endif}
{$endif}
