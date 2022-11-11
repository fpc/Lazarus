//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCLTHREAD_C_H
#define QLCLTHREAD_C_H

#include "qlclthread.h"
#include "pascalbind.h"

C_EXPORT QLCLThreadH QLCLThread_Create(QObjectH parent);
C_EXPORT void QLCLThread_Destroy(QLCLThreadH handle);
C_EXPORT void QLCLThread_override_run(QLCLThreadH handle, const QOverrideHook hook);
C_EXPORT int QLCLThread_exec(QLCLThreadH handle);

#endif
