//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsplitter_hook_c.h"

QSplitter_hookH QSplitter_hook_Create(QObjectH handle)
{
	return (QSplitter_hookH) new QSplitter_hook((QObject*)handle);
}

void QSplitter_hook_Destroy(QSplitter_hookH handle)
{
	delete (QSplitter_hook *)handle;
}

void QSplitter_hook_hook_splitterMoved(QSplitter_hookH handle, QHookH hook)
{
	((QSplitter_hook *)handle)->hook_splitterMoved(hook);
}

QSplitterHandle_hookH QSplitterHandle_hook_Create(QObjectH handle)
{
	return (QSplitterHandle_hookH) new QSplitterHandle_hook((QObject*)handle);
}

void QSplitterHandle_hook_Destroy(QSplitterHandle_hookH handle)
{
	delete (QSplitterHandle_hook *)handle;
}

