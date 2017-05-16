//******************************************************************************
//  Copyright (c) 2017 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMARGINS_C_H
#define QMARGINS_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QMarginsH QMargins_Create();
C_EXPORT QMarginsH QMargins_Create2(int left, int top, int right, int bottom);
C_EXPORT void QMargins_Destroy(QMarginsH handle);
C_EXPORT bool QMargins_isNull(QMarginsH handle);
C_EXPORT int QMargins_left(QMarginsH handle);
C_EXPORT int QMargins_top(QMarginsH handle);
C_EXPORT int QMargins_right(QMarginsH handle);
C_EXPORT int QMargins_bottom(QMarginsH handle);
C_EXPORT void QMargins_setLeft(QMarginsH handle, int left);
C_EXPORT void QMargins_setTop(QMarginsH handle, int top);
C_EXPORT void QMargins_setRight(QMarginsH handle, int right);
C_EXPORT void QMargins_setBottom(QMarginsH handle, int bottom);

#endif
