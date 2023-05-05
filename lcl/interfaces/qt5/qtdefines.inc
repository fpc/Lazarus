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
  {.$DEFINE QTACCESSIBILITY}
  {Use QtUseNativeEventLoop with care since it raises problem with dialogs.}
  {.$DEFINE QtUseNativeEventLoop}
  {$IFDEF DARWIN}
  {enable this define if you have problems when dialogs are shown behind application}
  {.$DEFINE TQTMESSAGEBOXUSEPARENT}
  {$ENDIF}
  {enable this define if you have Qt build with "QT_NO_SESSIONMANAGER" for example Somlabs i.MX8 with Wayland }
  {defined on Qt side in file "qtgui-config.h"}
  {.$DEFINE QT_NO_SESSIONMANAGER}
  {.$DEFINE QTDIALOGSUSEHTMLTEXT} {we use plain text for dialogs - if enabled then dialogs can contain html formatted text}
{$ENDIF}
