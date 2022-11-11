//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDESKTOPSERVICES_C_H
#define QDESKTOPSERVICES_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT bool QDesktopServices_openUrl(const QUrlH url);
C_EXPORT void QDesktopServices_setUrlHandler(PWideString scheme, QObjectH receiver, const char* method);
C_EXPORT void QDesktopServices_unsetUrlHandler(PWideString scheme);

#endif
