//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qslider_c.h"

QSliderH QSlider_Create(QWidgetH parent)
{
	return (QSliderH) new QSlider((QWidget*)parent);
}

void QSlider_Destroy(QSliderH handle)
{
	delete (QSlider *)handle;
}

QSliderH QSlider_Create2(Qt::Orientation orientation, QWidgetH parent)
{
	return (QSliderH) new QSlider(orientation, (QWidget*)parent);
}

void QSlider_sizeHint(QSliderH handle, PSize retval)
{
	*(QSize *)retval = ((QSlider *)handle)->sizeHint();
}

void QSlider_minimumSizeHint(QSliderH handle, PSize retval)
{
	*(QSize *)retval = ((QSlider *)handle)->minimumSizeHint();
}

void QSlider_setTickPosition(QSliderH handle, QSlider::TickPosition position)
{
	((QSlider *)handle)->setTickPosition(position);
}

QSlider::TickPosition QSlider_tickPosition(QSliderH handle)
{
	return (QSlider::TickPosition) ((QSlider *)handle)->tickPosition();
}

void QSlider_setTickInterval(QSliderH handle, int ti)
{
	((QSlider *)handle)->setTickInterval(ti);
}

int QSlider_tickInterval(QSliderH handle)
{
	return (int) ((QSlider *)handle)->tickInterval();
}

bool QSlider_event(QSliderH handle, QEventH event)
{
	return (bool) ((QSlider *)handle)->event((QEvent*)event);
}

