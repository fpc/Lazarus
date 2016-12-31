//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcoreapplication_hook_c.h"

QCoreApplication_hookH QCoreApplication_hook_Create(QObjectH handle)
{
	return (QCoreApplication_hookH) new QCoreApplication_hook((QObject*)handle);
}

void QCoreApplication_hook_Destroy(QCoreApplication_hookH handle)
{
	delete (QCoreApplication_hook *)handle;
}

void QCoreApplication_hook_hook_aboutToQuit(QCoreApplication_hookH handle, QHookH hook)
{
	((QCoreApplication_hook *)handle)->hook_aboutToQuit(hook);
}

void QCoreApplication_hook_hook_organizationNameChanged(QCoreApplication_hookH handle, QHookH hook)
{
	((QCoreApplication_hook *)handle)->hook_organizationNameChanged(hook);
}

void QCoreApplication_hook_hook_organizationDomainChanged(QCoreApplication_hookH handle, QHookH hook)
{
	((QCoreApplication_hook *)handle)->hook_organizationDomainChanged(hook);
}

void QCoreApplication_hook_hook_applicationNameChanged(QCoreApplication_hookH handle, QHookH hook)
{
	((QCoreApplication_hook *)handle)->hook_applicationNameChanged(hook);
}

void QCoreApplication_hook_hook_applicationVersionChanged(QCoreApplication_hookH handle, QHookH hook)
{
	((QCoreApplication_hook *)handle)->hook_applicationVersionChanged(hook);
}

