//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlcdnumber_hook_c.h"

QLCDNumber_hookH QLCDNumber_hook_Create(QObjectH handle)
{
	return (QLCDNumber_hookH) new QLCDNumber_hook((QObject*)handle);
}

void QLCDNumber_hook_Destroy(QLCDNumber_hookH handle)
{
	delete (QLCDNumber_hook *)handle;
}

void QLCDNumber_hook_hook_overflow(QLCDNumber_hookH handle, QHookH hook)
{
	((QLCDNumber_hook *)handle)->hook_overflow(hook);
}

