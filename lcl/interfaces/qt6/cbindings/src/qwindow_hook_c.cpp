//******************************************************************************
//  Copyright (c) 2017 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwindow_hook_c.h"

QWindow_hookH QWindow_hook_Create(QObjectH handle)
{
	return (QWindow_hookH) new QWindow_hook((QObject*)handle);
}

void QWindow_hook_Destroy(QWindow_hookH handle)
{
	delete (QWindow_hook *)handle;
}

void QWindow_hook_hook_screenChanged(QWindow_hookH handle, QHookH hook)
{
	((QWindow_hook *)handle)->hook_screenChanged(hook);
}

void QWindow_hook_hook_modalityChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_modalityChanged(hook);
}

void QWindow_hook_hook_windowStateChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_windowStateChanged(hook);
}

void QWindow_hook_hook_windowTitleChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_windowTitleChanged(hook);
}

void QWindow_hook_hook_xChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_xChanged(hook);
}

void QWindow_hook_hook_yChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_yChanged(hook);
}

void QWindow_hook_hook_widthChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_widthChanged(hook);
}

void QWindow_hook_hook_heightChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_heightChanged(hook);
}

void QWindow_hook_hook_minimumWidthChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_minimumWidthChanged(hook);
}

void QWindow_hook_hook_minimumHeightChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_minimumHeightChanged(hook);
}

void QWindow_hook_hook_maximumWidthChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_maximumWidthChanged(hook);
}

void QWindow_hook_hook_maximumHeightChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_maximumHeightChanged(hook);
}

void QWindow_hook_hook_visibleChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_visibleChanged(hook);
}

void QWindow_hook_hook_visibilityChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_visibilityChanged(hook);
}

void QWindow_hook_hook_activeChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_activeChanged(hook);
}

void QWindow_hook_hook_contentOrientationChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_contentOrientationChanged(hook);
}

void QWindow_hook_hook_focusObjectChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_focusObjectChanged(hook);
}

void QWindow_hook_hook_opacityChanged(QWindow_hookH handle, QHookH hook)
{
  ((QWindow_hook *)handle)->hook_opacityChanged(hook);
}
