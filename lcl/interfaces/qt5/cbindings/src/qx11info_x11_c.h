#ifndef QX11INFO_C_H
#define QX11INFO_C_H

#include <QX11Info>
#include "pascalbind.h"
/*#include <qx11info_x11.h> */

C_EXPORT bool QX11Info_isPlatformX11();
C_EXPORT int QX11Info_appDpiX(int screen);
C_EXPORT int QX11Info_appDpiY(int screen);
C_EXPORT unsigned long QX11Info_appRootWindow(int screen);
C_EXPORT int QX11Info_appScreen();
C_EXPORT unsigned long QX11Info_appTime();
C_EXPORT unsigned long QX11Info_appUserTime();
C_EXPORT void QX11Info_setAppTime(unsigned long time);
C_EXPORT void QX11Info_setAppUserTime(unsigned long time);
C_EXPORT unsigned long QX11Info_getTimestamp();
C_EXPORT QByteArray QX11Info_nextStartupId();
C_EXPORT void QX11Info_setNextStartupId(const QByteArray &id);
C_EXPORT Display* QX11Info_display();
#endif
