//******************************************************************************
//  Copyright (c) 2017-2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qscreen_hook_c.h"

QScreen_hookH QScreen_hook_Create(QObjectH handle)
{
	return (QScreen_hookH) new QScreen_hook((QObject*)handle);
}

void QScreen_hook_Destroy(QScreen_hookH handle)
{
	delete (QScreen_hook *)handle;
}

void QScreen_hook_hook_geometryChanged(QScreen_hookH handle, QHookH hook)
{
  ((QScreen_hook *)handle)->hook_geometryChanged(hook);
}

void QScreen_hook_hook_availableGeometryChanged(QScreen_hookH handle, QHookH hook)
{
  ((QScreen_hook *)handle)->hook_availableGeometryChanged(hook);
}

void QScreen_hook_hook_physicalSizeChanged(QScreen_hookH handle, QHookH hook)
{
  ((QScreen_hook *)handle)->hook_physicalSizeChanged(hook);
}

void QScreen_hook_hook_physicalDotsPerInchChanged(QScreen_hookH handle, QHookH hook)
{
  ((QScreen_hook *)handle)->hook_physicalDotsPerInchChanged(hook);
}

void QScreen_hook_hook_logicalDotsPerInchChanged(QScreen_hookH handle, QHookH hook)
{
  ((QScreen_hook *)handle)->hook_logicalDotsPerInchChanged(hook);
}

void QScreen_hook_hook_virtualGeometryChanged(QScreen_hookH handle, QHookH hook)
{
  ((QScreen_hook *)handle)->hook_virtualGeometryChanged(hook);
}

void QScreen_hook_hook_primaryOrientationChanged(QScreen_hookH handle, QHookH hook)
{
  ((QScreen_hook *)handle)->hook_primaryOrientationChanged(hook);
}

void QScreen_hook_hook_orientationChanged(QScreen_hookH handle, QHookH hook)
{
  ((QScreen_hook *)handle)->hook_orientationChanged(hook);
}

void QScreen_hook_hook_refreshRateChanged(QScreen_hookH handle, QHookH hook)
{
  ((QScreen_hook *)handle)->hook_refreshRateChanged(hook);
}

