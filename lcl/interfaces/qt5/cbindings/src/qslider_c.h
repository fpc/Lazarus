//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSLIDER_C_H
#define QSLIDER_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QSliderH QSlider_Create(QWidgetH parent);
C_EXPORT void QSlider_Destroy(QSliderH handle);
C_EXPORT QSliderH QSlider_Create2(Qt::Orientation orientation, QWidgetH parent);
C_EXPORT void QSlider_sizeHint(QSliderH handle, PSize retval);
C_EXPORT void QSlider_minimumSizeHint(QSliderH handle, PSize retval);
C_EXPORT void QSlider_setTickPosition(QSliderH handle, QSlider::TickPosition position);
C_EXPORT QSlider::TickPosition QSlider_tickPosition(QSliderH handle);
C_EXPORT void QSlider_setTickInterval(QSliderH handle, int ti);
C_EXPORT int QSlider_tickInterval(QSliderH handle);
C_EXPORT bool QSlider_event(QSliderH handle, QEventH event);

#endif
