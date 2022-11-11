//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPROGRESSBAR_C_H
#define QPROGRESSBAR_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QProgressBarH QProgressBar_Create(QWidgetH parent);
C_EXPORT void QProgressBar_Destroy(QProgressBarH handle);
C_EXPORT int QProgressBar_minimum(QProgressBarH handle);
C_EXPORT int QProgressBar_maximum(QProgressBarH handle);
C_EXPORT int QProgressBar_value(QProgressBarH handle);
C_EXPORT void QProgressBar_text(QProgressBarH handle, PWideString retval);
C_EXPORT void QProgressBar_setTextVisible(QProgressBarH handle, bool visible);
C_EXPORT bool QProgressBar_isTextVisible(QProgressBarH handle);
C_EXPORT unsigned int QProgressBar_alignment(QProgressBarH handle);
C_EXPORT void QProgressBar_setAlignment(QProgressBarH handle, unsigned int alignment);
C_EXPORT void QProgressBar_sizeHint(QProgressBarH handle, PSize retval);
C_EXPORT void QProgressBar_minimumSizeHint(QProgressBarH handle, PSize retval);
C_EXPORT Qt::Orientation QProgressBar_orientation(QProgressBarH handle);
C_EXPORT void QProgressBar_setInvertedAppearance(QProgressBarH handle, bool invert);
C_EXPORT bool QProgressBar_invertedAppearance(QProgressBarH handle);
C_EXPORT void QProgressBar_setTextDirection(QProgressBarH handle, QProgressBar::Direction textDirection);
C_EXPORT QProgressBar::Direction QProgressBar_textDirection(QProgressBarH handle);
C_EXPORT void QProgressBar_setFormat(QProgressBarH handle, PWideString format);
C_EXPORT void QProgressBar_resetFormat(QProgressBarH handle);
C_EXPORT void QProgressBar_format(QProgressBarH handle, PWideString retval);
C_EXPORT void QProgressBar_reset(QProgressBarH handle);
C_EXPORT void QProgressBar_setRange(QProgressBarH handle, int minimum, int maximum);
C_EXPORT void QProgressBar_setMinimum(QProgressBarH handle, int minimum);
C_EXPORT void QProgressBar_setMaximum(QProgressBarH handle, int maximum);
C_EXPORT void QProgressBar_setValue(QProgressBarH handle, int value);
C_EXPORT void QProgressBar_setOrientation(QProgressBarH handle, Qt::Orientation AnonParam1);

#endif
