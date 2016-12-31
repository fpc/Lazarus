//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qshortcut_hook_c.h"

QShortcut_hookH QShortcut_hook_Create(QObjectH handle)
{
	return (QShortcut_hookH) new QShortcut_hook((QObject*)handle);
}

void QShortcut_hook_Destroy(QShortcut_hookH handle)
{
	delete (QShortcut_hook *)handle;
}

void QShortcut_hook_hook_activated(QShortcut_hookH handle, QHookH hook)
{
	((QShortcut_hook *)handle)->hook_activated(hook);
}

void QShortcut_hook_hook_activatedAmbiguously(QShortcut_hookH handle, QHookH hook)
{
	((QShortcut_hook *)handle)->hook_activatedAmbiguously(hook);
}

