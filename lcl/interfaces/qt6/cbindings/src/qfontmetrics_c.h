//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFONTMETRICS_C_H
#define QFONTMETRICS_C_H

#include <QtGui>
#include "pascalbind.h"

// TODO add QStringH variants to QFontMetricsF horizontalAdvance, boundingRect, size, tightBoundingRect, elidedText
// Idea is to have all function variants with QString for painting since then WideString->QString copying won't occur
// and we''ll have faster text painting routines. QFontMetrics already have all, must set QPainter variants.

C_EXPORT QFontMetricsH QFontMetrics_Create(const QFontH AnonParam1);
C_EXPORT void QFontMetrics_Destroy(QFontMetricsH handle);
C_EXPORT QFontMetricsH QFontMetrics_Create2(const QFontH AnonParam1, QPaintDeviceH pd);
C_EXPORT QFontMetricsH QFontMetrics_Create3(const QFontMetricsH AnonParam1);
C_EXPORT void QFontMetrics_swap(QFontMetricsH handle, QFontMetricsH other);
C_EXPORT int QFontMetrics_ascent(QFontMetricsH handle);
C_EXPORT int QFontMetrics_capHeight(QFontMetricsH handle);
C_EXPORT int QFontMetrics_descent(QFontMetricsH handle);
C_EXPORT qreal QFontMetrics_fontDpi(QFontMetricsH handle);
C_EXPORT int QFontMetrics_height(QFontMetricsH handle);
C_EXPORT int QFontMetrics_leading(QFontMetricsH handle);
C_EXPORT int QFontMetrics_lineSpacing(QFontMetricsH handle);
C_EXPORT int QFontMetrics_minLeftBearing(QFontMetricsH handle);
C_EXPORT int QFontMetrics_minRightBearing(QFontMetricsH handle);
C_EXPORT int QFontMetrics_maxWidth(QFontMetricsH handle);
C_EXPORT int QFontMetrics_xHeight(QFontMetricsH handle);
C_EXPORT int QFontMetrics_averageCharWidth(QFontMetricsH handle);
C_EXPORT bool QFontMetrics_inFont(QFontMetricsH handle, PWideChar AnonParam1);
C_EXPORT bool QFontMetrics_inFontUcs4(QFontMetricsH handle, uint ucs4);
C_EXPORT int QFontMetrics_leftBearing(QFontMetricsH handle, PWideChar AnonParam1);
C_EXPORT int QFontMetrics_rightBearing(QFontMetricsH handle, PWideChar AnonParam1);
C_EXPORT int QFontMetrics_horizontalAdvance(QFontMetricsH handle, PWideString AnonParam1, int len);
C_EXPORT int QFontMetrics_horizontalAdvance2(QFontMetricsH handle, PWideChar AnonParam1);

C_EXPORT void QFontMetrics_boundingRect(QFontMetricsH handle, PRect retval, PWideChar AnonParam1);
C_EXPORT void QFontMetrics_boundingRect2(QFontMetricsH handle, PRect retval, PWideString text);
C_EXPORT void QFontMetrics_boundingRect3(QFontMetricsH handle, PRect retval, PRect r, int flags, PWideString text, int tabstops, int* tabarray);
C_EXPORT void QFontMetrics_boundingRect4(QFontMetricsH handle, PRect retval, int x, int y, int w, int h, int flags, PWideString text, int tabstops, int* tabarray);
C_EXPORT void QFontMetrics_size(QFontMetricsH handle, PSize retval, int flags, PWideString str, int tabstops, int* tabarray);
C_EXPORT void QFontMetrics_tightBoundingRect(QFontMetricsH handle, PRect retval, PWideString text);
C_EXPORT void QFontMetrics_elidedText(QFontMetricsH handle, PWideString retval, PWideString text, Qt::TextElideMode mode, int width, int flags);

C_EXPORT int QFontMetrics_horizontalAdvance3(QFontMetricsH handle, const QStringH text, int len);
C_EXPORT void QFontMetrics_boundingRect5(QFontMetricsH handle, PRect retval, const QStringH text);
C_EXPORT void QFontMetrics_boundingRect6(QFontMetricsH handle, PRect retval, PRect r, int flags, const QStringH text, int tabstops, int* tabarray);
C_EXPORT void QFontMetrics_boundingRect7(QFontMetricsH handle, PRect retval, int x, int y, int w, int h, int flags, const QStringH text, int tabstops, int* tabarray);
C_EXPORT void QFontMetrics_size2(QFontMetricsH handle, PSize retval, int flags, const QStringH str, int tabstops, int* tabarray);
C_EXPORT void QFontMetrics_tightBoundingRect2(QFontMetricsH handle, PRect retval, const QStringH text);
C_EXPORT void QFontMetrics_elidedText2(QFontMetricsH handle, QStringH retval, const QStringH text, Qt::TextElideMode mode, int width, int flags);

C_EXPORT int QFontMetrics_underlinePos(QFontMetricsH handle);
C_EXPORT int QFontMetrics_overlinePos(QFontMetricsH handle);
C_EXPORT int QFontMetrics_strikeOutPos(QFontMetricsH handle);
C_EXPORT int QFontMetrics_lineWidth(QFontMetricsH handle);

C_EXPORT QFontMetricsFH QFontMetricsF_Create(const QFontH AnonParam1);
C_EXPORT void QFontMetricsF_Destroy(QFontMetricsFH handle);
C_EXPORT QFontMetricsFH QFontMetricsF_Create2(const QFontH AnonParam1, QPaintDeviceH pd);
C_EXPORT QFontMetricsFH QFontMetricsF_Create4(const QFontMetricsFH AnonParam1);
C_EXPORT void QFontMetricsF_swap(QFontMetricsFH handle, QFontMetricsFH other);
C_EXPORT qreal QFontMetricsF_ascent(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_capHeight(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_descent(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_fontDpi(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_height(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_leading(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_lineSpacing(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_minLeftBearing(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_minRightBearing(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_maxWidth(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_xHeight(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_averageCharWidth(QFontMetricsFH handle);
C_EXPORT bool QFontMetricsF_inFont(QFontMetricsFH handle, PWideChar AnonParam1);
C_EXPORT bool QFontMetricsF_inFontUcs4(QFontMetricsFH handle, uint ucs4);
C_EXPORT qreal QFontMetricsF_leftBearing(QFontMetricsFH handle, PWideChar AnonParam1);
C_EXPORT qreal QFontMetricsF_rightBearing(QFontMetricsFH handle, PWideChar AnonParam1);
C_EXPORT qreal QFontMetricsF_horizontalAdvance(QFontMetricsH handle, PWideString AnonParam1, int len);
C_EXPORT qreal QFontMetricsF_horizontalAdvance2(QFontMetricsH handle, PWideChar AnonParam1);
C_EXPORT void QFontMetricsF_boundingRect(QFontMetricsFH handle, QRectFH retval, PWideString string);
C_EXPORT void QFontMetricsF_boundingRect2(QFontMetricsFH handle, QRectFH retval, PWideChar AnonParam1);
C_EXPORT void QFontMetricsF_boundingRect3(QFontMetricsFH handle, QRectFH retval, const QRectFH r, int flags, PWideString string, int tabstops, int* tabarray);
C_EXPORT void QFontMetricsF_size(QFontMetricsFH handle, QSizeFH retval, int flags, PWideString str, int tabstops, int* tabarray);
C_EXPORT void QFontMetricsF_tightBoundingRect(QFontMetricsFH handle, QRectFH retval, PWideString text);
C_EXPORT void QFontMetricsF_elidedText(QFontMetricsFH handle, PWideString retval, PWideString text, Qt::TextElideMode mode, qreal width, int flags);

C_EXPORT int QFontMetricsF_horizontalAdvance3(QFontMetricsH handle, const QStringH text, int len);
C_EXPORT void QFontMetricsF_boundingRect5(QFontMetricsH handle, QRectFH retval, const QStringH text);
C_EXPORT void QFontMetricsF_boundingRect6(QFontMetricsH handle, QRectFH retval, QRectFH r, int flags, const QStringH text, int tabstops, int* tabarray);
C_EXPORT void QFontMetricsF_size2(QFontMetricsH handle, QSizeFH retval, int flags, const QStringH str, int tabstops, int* tabarray);
C_EXPORT void QFontMetricsF_tightBoundingRect2(QFontMetricsH handle, QRectFH retval, const QStringH text);
C_EXPORT void QFontMetricsF_elidedText2(QFontMetricsH handle, QStringH retval, const QStringH text, Qt::TextElideMode mode, int width, int flags);

C_EXPORT qreal QFontMetricsF_underlinePos(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_overlinePos(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_strikeOutPos(QFontMetricsFH handle);
C_EXPORT qreal QFontMetricsF_lineWidth(QFontMetricsFH handle);

#endif
