//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qscrollbar_hook_c.h"

QScrollBar_hookH QScrollBar_hook_Create(QObjectH handle)
{
	return (QScrollBar_hookH) new QScrollBar_hook((QObject*)handle);
}

void QScrollBar_hook_Destroy(QScrollBar_hookH handle)
{
	delete (QScrollBar_hook *)handle;
}

