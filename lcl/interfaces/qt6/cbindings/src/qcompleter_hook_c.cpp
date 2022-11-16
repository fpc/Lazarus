//******************************************************************************
//  Copyright (c) 2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcompleter_hook_c.h"

QCompleter_hookH QCompleter_hook_Create(QObjectH handle)
{
	return (QCompleter_hookH) new QCompleter_hook((QObject*)handle);
}

void QCompleter_hook_Destroy(QCompleter_hookH handle)
{
	delete (QCompleter_hook *)handle;
}

void QCompleter_hook_hook_activated(QCompleter_hookH handle, QHookH hook)
{
	((QCompleter_hook *)handle)->hook_activated(hook);
}

void QCompleter_hook_hook_activated2(QCompleter_hookH handle, QHookH hook)
{
	((QCompleter_hook *)handle)->hook_activated2(hook);
}

void QCompleter_hook_hook_highlighted(QCompleter_hookH handle, QHookH hook)
{
	((QCompleter_hook *)handle)->hook_highlighted(hook);
}

void QCompleter_hook_hook_highlighted2(QCompleter_hookH handle, QHookH hook)
{
	((QCompleter_hook *)handle)->hook_highlighted2(hook);
}



