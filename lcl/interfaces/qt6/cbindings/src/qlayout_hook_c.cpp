//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlayout_hook_c.h"

QLayout_hookH QLayout_hook_Create(QObjectH handle)
{
	return (QLayout_hookH) new QLayout_hook((QObject*)handle);
}

void QLayout_hook_Destroy(QLayout_hookH handle)
{
	delete (QLayout_hook *)handle;
}

