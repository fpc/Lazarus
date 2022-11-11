//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qframe_hook_c.h"

QFrame_hookH QFrame_hook_Create(QObjectH handle)
{
	return (QFrame_hookH) new QFrame_hook((QObject*)handle);
}

void QFrame_hook_Destroy(QFrame_hookH handle)
{
	delete (QFrame_hook *)handle;
}

