//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSLIDER_C_H
#define QABSTRACTSLIDER_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QAbstractSliderH QAbstractSlider_Create(QWidgetH parent);
C_EXPORT void QAbstractSlider_Destroy(QAbstractSliderH handle);
C_EXPORT Qt::Orientation QAbstractSlider_orientation(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_setMinimum(QAbstractSliderH handle, int AnonParam1);
C_EXPORT int QAbstractSlider_minimum(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_setMaximum(QAbstractSliderH handle, int AnonParam1);
C_EXPORT int QAbstractSlider_maximum(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_setSingleStep(QAbstractSliderH handle, int AnonParam1);
C_EXPORT int QAbstractSlider_singleStep(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_setPageStep(QAbstractSliderH handle, int AnonParam1);
C_EXPORT int QAbstractSlider_pageStep(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_setTracking(QAbstractSliderH handle, bool enable);
C_EXPORT bool QAbstractSlider_hasTracking(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_setSliderDown(QAbstractSliderH handle, bool AnonParam1);
C_EXPORT bool QAbstractSlider_isSliderDown(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_setSliderPosition(QAbstractSliderH handle, int AnonParam1);
C_EXPORT int QAbstractSlider_sliderPosition(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_setInvertedAppearance(QAbstractSliderH handle, bool AnonParam1);
C_EXPORT bool QAbstractSlider_invertedAppearance(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_setInvertedControls(QAbstractSliderH handle, bool AnonParam1);
C_EXPORT bool QAbstractSlider_invertedControls(QAbstractSliderH handle);
C_EXPORT int QAbstractSlider_value(QAbstractSliderH handle);
C_EXPORT void QAbstractSlider_triggerAction(QAbstractSliderH handle, QAbstractSlider::SliderAction action);
C_EXPORT void QAbstractSlider_setValue(QAbstractSliderH handle, int AnonParam1);
C_EXPORT void QAbstractSlider_setOrientation(QAbstractSliderH handle, Qt::Orientation AnonParam1);
C_EXPORT void QAbstractSlider_setRange(QAbstractSliderH handle, int min, int max);

#endif
