//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebsecurityorigin_hook_c.h"

QWebSecurityOrigin_hookH QWebSecurityOrigin_hook_Create(QObjectH handle)
{
	return (QWebSecurityOrigin_hookH) new QWebSecurityOrigin_hook((QObject*)handle);
}

void QWebSecurityOrigin_hook_Destroy(QWebSecurityOrigin_hookH handle)
{
	delete (QWebSecurityOrigin_hook *)handle;
}

