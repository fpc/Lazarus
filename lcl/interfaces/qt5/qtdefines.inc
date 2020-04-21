{******************************************************************************
  Centralized includefile for some common qt5 defines
 ******************************************************************************}
{$IFNDEF QTOPIA}
  {$IF DEFINED(LINUX) or DEFINED(FREEBSD) or DEFINED(NETBSD)}
    {$DEFINE HASX11}
  {$ENDIF}
  {$IF DEFINED(DARWIN) AND DEFINED(CPU64)}
  {$DEFINE QTCOCOA}
  {$ENDIF}
  {$DEFINE QTSCROLLABLEFORMS}
  {Qt must use native event loop at least under MacOSX cocoa 64}
  {$IFNDEF HASX11}
  {$DEFINE QtUseNativeEventLoop}
  {$ENDIF}
  {$IFDEF DARWIN}
  {enable this define if you have problems when dialogs are shown behind application}
  {.$DEFINE TQTMESSAGEBOXUSEPARENT}
  {$ENDIF}
{$ENDIF}
