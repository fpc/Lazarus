//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprogressbar_c.h"

QProgressBarH QProgressBar_Create(QWidgetH parent)
{
	return (QProgressBarH) new QProgressBar((QWidget*)parent);
}

void QProgressBar_Destroy(QProgressBarH handle)
{
	delete (QProgressBar *)handle;
}

int QProgressBar_minimum(QProgressBarH handle)
{
	return (int) ((QProgressBar *)handle)->minimum();
}

int QProgressBar_maximum(QProgressBarH handle)
{
	return (int) ((QProgressBar *)handle)->maximum();
}

int QProgressBar_value(QProgressBarH handle)
{
	return (int) ((QProgressBar *)handle)->value();
}

void QProgressBar_text(QProgressBarH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QProgressBar *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QProgressBar_setTextVisible(QProgressBarH handle, bool visible)
{
	((QProgressBar *)handle)->setTextVisible(visible);
}

bool QProgressBar_isTextVisible(QProgressBarH handle)
{
	return (bool) ((QProgressBar *)handle)->isTextVisible();
}

unsigned int QProgressBar_alignment(QProgressBarH handle)
{
	return (unsigned int) ((QProgressBar *)handle)->alignment();
}

void QProgressBar_setAlignment(QProgressBarH handle, unsigned int alignment)
{
	((QProgressBar *)handle)->setAlignment((Qt::Alignment)alignment);
}

void QProgressBar_sizeHint(QProgressBarH handle, PSize retval)
{
	*(QSize *)retval = ((QProgressBar *)handle)->sizeHint();
}

void QProgressBar_minimumSizeHint(QProgressBarH handle, PSize retval)
{
	*(QSize *)retval = ((QProgressBar *)handle)->minimumSizeHint();
}

Qt::Orientation QProgressBar_orientation(QProgressBarH handle)
{
	return (Qt::Orientation) ((QProgressBar *)handle)->orientation();
}

void QProgressBar_setInvertedAppearance(QProgressBarH handle, bool invert)
{
	((QProgressBar *)handle)->setInvertedAppearance(invert);
}

bool QProgressBar_invertedAppearance(QProgressBarH handle)
{
	return (bool) ((QProgressBar *)handle)->invertedAppearance();
}

void QProgressBar_setTextDirection(QProgressBarH handle, QProgressBar::Direction textDirection)
{
	((QProgressBar *)handle)->setTextDirection(textDirection);
}

QProgressBar::Direction QProgressBar_textDirection(QProgressBarH handle)
{
	return (QProgressBar::Direction) ((QProgressBar *)handle)->textDirection();
}

void QProgressBar_setFormat(QProgressBarH handle, PWideString format)
{
	QString t_format;
	copyPWideStringToQString(format, t_format);
	((QProgressBar *)handle)->setFormat(t_format);
}

void QProgressBar_resetFormat(QProgressBarH handle)
{
	((QProgressBar *)handle)->resetFormat();
}

void QProgressBar_format(QProgressBarH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QProgressBar *)handle)->format();
	copyQStringToPWideString(t_retval, retval);
}

void QProgressBar_reset(QProgressBarH handle)
{
	((QProgressBar *)handle)->reset();
}

void QProgressBar_setRange(QProgressBarH handle, int minimum, int maximum)
{
	((QProgressBar *)handle)->setRange(minimum, maximum);
}

void QProgressBar_setMinimum(QProgressBarH handle, int minimum)
{
	((QProgressBar *)handle)->setMinimum(minimum);
}

void QProgressBar_setMaximum(QProgressBarH handle, int maximum)
{
	((QProgressBar *)handle)->setMaximum(maximum);
}

void QProgressBar_setValue(QProgressBarH handle, int value)
{
	((QProgressBar *)handle)->setValue(value);
}

void QProgressBar_setOrientation(QProgressBarH handle, Qt::Orientation AnonParam1)
{
	((QProgressBar *)handle)->setOrientation(AnonParam1);
}

