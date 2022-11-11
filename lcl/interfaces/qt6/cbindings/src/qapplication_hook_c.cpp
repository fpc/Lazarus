//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qapplication_hook_c.h"

QApplication_hookH QApplication_hook_Create(QObjectH handle)
{
	return (QApplication_hookH) new QApplication_hook((QObject*)handle);
}

void QApplication_hook_Destroy(QApplication_hookH handle)
{
	delete (QApplication_hook *)handle;
}

void QApplication_hook_hook_focusChanged(QApplication_hookH handle, QHookH hook)
{
	((QApplication_hook *)handle)->hook_focusChanged(hook);
}

