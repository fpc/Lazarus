//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSIZE_C_H
#define QSIZE_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QSizeH QSize_Create();
C_EXPORT void QSize_Destroy(QSizeH handle);
C_EXPORT QSizeH QSize_Create2(int w, int h);
C_EXPORT bool QSize_isNull(QSizeH handle);
C_EXPORT bool QSize_isEmpty(QSizeH handle);
C_EXPORT bool QSize_isValid(QSizeH handle);
C_EXPORT int QSize_width(QSizeH handle);
C_EXPORT int QSize_height(QSizeH handle);
C_EXPORT void QSize_setWidth(QSizeH handle, int w);
C_EXPORT void QSize_setHeight(QSizeH handle, int h);
C_EXPORT void QSize_transpose(QSizeH handle);
C_EXPORT void QSize_transposed(QSizeH handle, PSize retval);
C_EXPORT void QSize_scale(QSizeH handle, int w, int h, Qt::AspectRatioMode mode);
C_EXPORT void QSize_scale2(QSizeH handle, const QSizeH s, Qt::AspectRatioMode mode);
C_EXPORT void QSize_scaled(QSizeH handle, PSize retval, int w, int h, Qt::AspectRatioMode mode);
C_EXPORT void QSize_scaled2(QSizeH handle, PSize retval, const QSizeH s, Qt::AspectRatioMode mode);
C_EXPORT void QSize_expandedTo(QSizeH handle, PSize retval, const QSizeH AnonParam1);
C_EXPORT void QSize_boundedTo(QSizeH handle, PSize retval, const QSizeH AnonParam1);
C_EXPORT int* QSize_rwidth(QSizeH handle);
C_EXPORT int* QSize_rheight(QSizeH handle);
C_EXPORT QSizeFH QSizeF_Create();
C_EXPORT void QSizeF_Destroy(QSizeFH handle);
C_EXPORT QSizeFH QSizeF_Create2(const QSizeH sz);
C_EXPORT QSizeFH QSizeF_Create3(qreal w, qreal h);
C_EXPORT bool QSizeF_isNull(QSizeFH handle);
C_EXPORT bool QSizeF_isEmpty(QSizeFH handle);
C_EXPORT bool QSizeF_isValid(QSizeFH handle);
C_EXPORT qreal QSizeF_width(QSizeFH handle);
C_EXPORT qreal QSizeF_height(QSizeFH handle);
C_EXPORT void QSizeF_setWidth(QSizeFH handle, qreal w);
C_EXPORT void QSizeF_setHeight(QSizeFH handle, qreal h);
C_EXPORT void QSizeF_transpose(QSizeFH handle);
C_EXPORT void QSizeF_transposed(QSizeFH handle, QSizeFH retval);
C_EXPORT void QSizeF_scale(QSizeFH handle, qreal w, qreal h, Qt::AspectRatioMode mode);
C_EXPORT void QSizeF_scale2(QSizeFH handle, const QSizeFH s, Qt::AspectRatioMode mode);
C_EXPORT void QSizeF_scaled(QSizeFH handle, QSizeFH retval, qreal w, qreal h, Qt::AspectRatioMode mode);
C_EXPORT void QSizeF_scaled2(QSizeFH handle, QSizeFH retval, const QSizeFH s, Qt::AspectRatioMode mode);
C_EXPORT void QSizeF_expandedTo(QSizeFH handle, QSizeFH retval, const QSizeFH AnonParam1);
C_EXPORT void QSizeF_boundedTo(QSizeFH handle, QSizeFH retval, const QSizeFH AnonParam1);
C_EXPORT qreal* QSizeF_rwidth(QSizeFH handle);
C_EXPORT qreal* QSizeF_rheight(QSizeFH handle);
C_EXPORT void QSizeF_toSize(QSizeFH handle, PSize retval);

#endif
