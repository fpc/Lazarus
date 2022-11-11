//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebsettings_hook_c.h"

QWebSettings_hookH QWebSettings_hook_Create(QObjectH handle)
{
	return (QWebSettings_hookH) new QWebSettings_hook((QObject*)handle);
}

void QWebSettings_hook_Destroy(QWebSettings_hookH handle)
{
	delete (QWebSettings_hook *)handle;
}

