//******************************************************************************
//  Copyright (c) 2005-2011 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#if defined BINUX || MAEMO5
#include "qx11info_x11_c.h"
#endif
#if defined BINUX

bool QX11Info_isPlatformX11()
{
    return (bool) QX11Info::isPlatformX11();
}

int QX11Info_appDpiX(int screen)
{
	return (int) QX11Info::appDpiX(screen);
}

int QX11Info_appDpiY(int screen)
{
	return (int) QX11Info::appDpiY(screen);
}

unsigned long QX11Info_appRootWindow(int screen)
{
	return (unsigned long) QX11Info::appRootWindow(screen);
}

int QX11Info_appScreen()
{
	return (int) QX11Info::appScreen();
}

unsigned long QX11Info_appTime()
{
	return (unsigned long) QX11Info::appTime();
}

unsigned long QX11Info_appUserTime()
{
	return (unsigned long) QX11Info::appUserTime();
}

void QX11Info_setAppTime(unsigned long time)
{
	QX11Info::setAppTime(time);
}

void QX11Info_setAppUserTime(unsigned long time)
{
	QX11Info::setAppUserTime(time);
}

unsigned long QX11Info_getTimestamp()
{
	return (unsigned long) QX11Info::getTimestamp();
}

QByteArray QX11Info_nextStartupId()
{
	return (QByteArray) QX11Info::nextStartupId();
}

void QX11Info_setNextStartupId(const QByteArray &id)
{
  QX11Info::setNextStartupId(id);    
}

Display* QX11Info_display()
{
	return (Display*) QX11Info::display();
}

#endif
