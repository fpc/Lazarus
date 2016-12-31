//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qdesktopservices_c.h"

bool QDesktopServices_openUrl(const QUrlH url)
{
	return (bool) QDesktopServices::openUrl(*(const QUrl*)url);
}

void QDesktopServices_setUrlHandler(PWideString scheme, QObjectH receiver, const char* method)
{
	QString t_scheme;
	copyPWideStringToQString(scheme, t_scheme);
	QDesktopServices::setUrlHandler(t_scheme, (QObject*)receiver, method);
}

void QDesktopServices_unsetUrlHandler(PWideString scheme)
{
	QString t_scheme;
	copyPWideStringToQString(scheme, t_scheme);
	QDesktopServices::unsetUrlHandler(t_scheme);
}

