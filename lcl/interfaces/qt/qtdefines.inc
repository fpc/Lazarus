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
  {$IFNDEF QTCOCOA}
  {$DEFINE QTSCROLLABLEFORMS}
  {$ENDIF}
{$ENDIF}
