//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlclthread_c.h"

QLCLThreadH QLCLThread_Create(QObjectH parent)
{
	return (QLCLThreadH) new QLCLThread((QObject*)parent);
}

void QLCLThread_Destroy(QLCLThreadH handle)
{
	delete (QLCLThread *)handle;
}

void QLCLThread_override_run(QLCLThreadH handle, const QOverrideHook hook)
{
	((QLCLThread *)handle)->override_run(hook);
}

int QLCLThread_exec(QLCLThreadH handle)
{
	return (int) ((QLCLThread *)handle)->exec();
}

