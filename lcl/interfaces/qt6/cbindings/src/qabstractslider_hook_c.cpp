//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractslider_hook_c.h"

QAbstractSlider_hookH QAbstractSlider_hook_Create(QObjectH handle)
{
	return (QAbstractSlider_hookH) new QAbstractSlider_hook((QObject*)handle);
}

void QAbstractSlider_hook_Destroy(QAbstractSlider_hookH handle)
{
	delete (QAbstractSlider_hook *)handle;
}

void QAbstractSlider_hook_hook_valueChanged(QAbstractSlider_hookH handle, QHookH hook)
{
	((QAbstractSlider_hook *)handle)->hook_valueChanged(hook);
}

void QAbstractSlider_hook_hook_sliderPressed(QAbstractSlider_hookH handle, QHookH hook)
{
	((QAbstractSlider_hook *)handle)->hook_sliderPressed(hook);
}

void QAbstractSlider_hook_hook_sliderMoved(QAbstractSlider_hookH handle, QHookH hook)
{
	((QAbstractSlider_hook *)handle)->hook_sliderMoved(hook);
}

void QAbstractSlider_hook_hook_sliderReleased(QAbstractSlider_hookH handle, QHookH hook)
{
	((QAbstractSlider_hook *)handle)->hook_sliderReleased(hook);
}

void QAbstractSlider_hook_hook_rangeChanged(QAbstractSlider_hookH handle, QHookH hook)
{
	((QAbstractSlider_hook *)handle)->hook_rangeChanged(hook);
}

void QAbstractSlider_hook_hook_actionTriggered(QAbstractSlider_hookH handle, QHookH hook)
{
	((QAbstractSlider_hook *)handle)->hook_actionTriggered(hook);
}

