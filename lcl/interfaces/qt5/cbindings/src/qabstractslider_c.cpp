//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractslider_c.h"

QAbstractSliderH QAbstractSlider_Create(QWidgetH parent)
{
	return (QAbstractSliderH) new QAbstractSlider((QWidget*)parent);
}

void QAbstractSlider_Destroy(QAbstractSliderH handle)
{
	delete (QAbstractSlider *)handle;
}

Qt::Orientation QAbstractSlider_orientation(QAbstractSliderH handle)
{
	return (Qt::Orientation) ((QAbstractSlider *)handle)->orientation();
}

void QAbstractSlider_setMinimum(QAbstractSliderH handle, int AnonParam1)
{
	((QAbstractSlider *)handle)->setMinimum(AnonParam1);
}

int QAbstractSlider_minimum(QAbstractSliderH handle)
{
	return (int) ((QAbstractSlider *)handle)->minimum();
}

void QAbstractSlider_setMaximum(QAbstractSliderH handle, int AnonParam1)
{
	((QAbstractSlider *)handle)->setMaximum(AnonParam1);
}

int QAbstractSlider_maximum(QAbstractSliderH handle)
{
	return (int) ((QAbstractSlider *)handle)->maximum();
}

void QAbstractSlider_setSingleStep(QAbstractSliderH handle, int AnonParam1)
{
	((QAbstractSlider *)handle)->setSingleStep(AnonParam1);
}

int QAbstractSlider_singleStep(QAbstractSliderH handle)
{
	return (int) ((QAbstractSlider *)handle)->singleStep();
}

void QAbstractSlider_setPageStep(QAbstractSliderH handle, int AnonParam1)
{
	((QAbstractSlider *)handle)->setPageStep(AnonParam1);
}

int QAbstractSlider_pageStep(QAbstractSliderH handle)
{
	return (int) ((QAbstractSlider *)handle)->pageStep();
}

void QAbstractSlider_setTracking(QAbstractSliderH handle, bool enable)
{
	((QAbstractSlider *)handle)->setTracking(enable);
}

bool QAbstractSlider_hasTracking(QAbstractSliderH handle)
{
	return (bool) ((QAbstractSlider *)handle)->hasTracking();
}

void QAbstractSlider_setSliderDown(QAbstractSliderH handle, bool AnonParam1)
{
	((QAbstractSlider *)handle)->setSliderDown(AnonParam1);
}

bool QAbstractSlider_isSliderDown(QAbstractSliderH handle)
{
	return (bool) ((QAbstractSlider *)handle)->isSliderDown();
}

void QAbstractSlider_setSliderPosition(QAbstractSliderH handle, int AnonParam1)
{
	((QAbstractSlider *)handle)->setSliderPosition(AnonParam1);
}

int QAbstractSlider_sliderPosition(QAbstractSliderH handle)
{
	return (int) ((QAbstractSlider *)handle)->sliderPosition();
}

void QAbstractSlider_setInvertedAppearance(QAbstractSliderH handle, bool AnonParam1)
{
	((QAbstractSlider *)handle)->setInvertedAppearance(AnonParam1);
}

bool QAbstractSlider_invertedAppearance(QAbstractSliderH handle)
{
	return (bool) ((QAbstractSlider *)handle)->invertedAppearance();
}

void QAbstractSlider_setInvertedControls(QAbstractSliderH handle, bool AnonParam1)
{
	((QAbstractSlider *)handle)->setInvertedControls(AnonParam1);
}

bool QAbstractSlider_invertedControls(QAbstractSliderH handle)
{
	return (bool) ((QAbstractSlider *)handle)->invertedControls();
}

int QAbstractSlider_value(QAbstractSliderH handle)
{
	return (int) ((QAbstractSlider *)handle)->value();
}

void QAbstractSlider_triggerAction(QAbstractSliderH handle, QAbstractSlider::SliderAction action)
{
	((QAbstractSlider *)handle)->triggerAction(action);
}

void QAbstractSlider_setValue(QAbstractSliderH handle, int AnonParam1)
{
	((QAbstractSlider *)handle)->setValue(AnonParam1);
}

void QAbstractSlider_setOrientation(QAbstractSliderH handle, Qt::Orientation AnonParam1)
{
	((QAbstractSlider *)handle)->setOrientation(AnonParam1);
}

void QAbstractSlider_setRange(QAbstractSliderH handle, int min, int max)
{
	((QAbstractSlider *)handle)->setRange(min, max);
}

