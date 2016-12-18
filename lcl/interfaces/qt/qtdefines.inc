{******************************************************************************
  Centralized includefile for some common qt defines
 ******************************************************************************}
{$IFNDEF QTOPIA}
  {$IF DEFINED(LINUX) or DEFINED(FREEBSD) or DEFINED(NETBSD)}
    {$DEFINE HASX11}
  {$ENDIF}
  {$IF DEFINED(DARWIN) AND DEFINED(CPU64)}
  {$DEFINE QTCOCOA}
  {$ENDIF}
  {$DEFINE QTSCROLLABLEFORMS}
  {$DEFINE QtUseNativeEventLoop}
  {$IFDEF DARWIN}
  {enable this define if you have problems when dialogs are shown behind application}
  {.$DEFINE TQTMESSAGEBOXUSEPARENT}
  {$ENDIF}
  {$IFDEF HASX11}
  {.$DEFINE QtUseAccurateFrame}
  {$ENDIF}
{$ENDIF}
