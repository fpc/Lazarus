//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfontmetrics_c.h"

QFontMetricsH QFontMetrics_Create(const QFontH AnonParam1)
{
	return (QFontMetricsH) new QFontMetrics(*(const QFont*)AnonParam1);
}

void QFontMetrics_Destroy(QFontMetricsH handle)
{
	delete (QFontMetrics *)handle;
}

QFontMetricsH QFontMetrics_Create2(const QFontH AnonParam1, QPaintDeviceH pd)
{
	return (QFontMetricsH) new QFontMetrics(*(const QFont*)AnonParam1, (QPaintDevice*)pd);
}

QFontMetricsH QFontMetrics_Create3(const QFontMetricsH AnonParam1)
{
	return (QFontMetricsH) new QFontMetrics(*(const QFontMetrics*)AnonParam1);
}

void QFontMetrics_swap(QFontMetricsH handle, QFontMetricsH other)
{
	((QFontMetrics *)handle)->swap(*(QFontMetrics*)other);
}

int QFontMetrics_ascent(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->ascent();
}

int QFontMetrics_capHeight(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->capHeight();
}

int QFontMetrics_descent(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->descent();
}

qreal QFontMetrics_fontDpi(QFontMetricsH handle)
{
	return (qreal) ((QFontMetrics *)handle)->fontDpi();
}

int QFontMetrics_height(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->height();
}

int QFontMetrics_leading(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->leading();
}

int QFontMetrics_lineSpacing(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->lineSpacing();
}

int QFontMetrics_minLeftBearing(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->minLeftBearing();
}

int QFontMetrics_minRightBearing(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->minRightBearing();
}

int QFontMetrics_maxWidth(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->maxWidth();
}

int QFontMetrics_xHeight(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->xHeight();
}

int QFontMetrics_averageCharWidth(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->averageCharWidth();
}

bool QFontMetrics_inFont(QFontMetricsH handle, PWideChar AnonParam1)
{
	return (bool) ((QFontMetrics *)handle)->inFont(*(QChar *)AnonParam1);
}

bool QFontMetrics_inFontUcs4(QFontMetricsH handle, uint ucs4)
{
	return (bool) ((QFontMetrics *)handle)->inFontUcs4(ucs4);
}

int QFontMetrics_leftBearing(QFontMetricsH handle, PWideChar AnonParam1)
{
	return (int) ((QFontMetrics *)handle)->leftBearing(*(QChar *)AnonParam1);
}

int QFontMetrics_rightBearing(QFontMetricsH handle, PWideChar AnonParam1)
{
	return (int) ((QFontMetrics *)handle)->rightBearing(*(QChar *)AnonParam1);
}

int QFontMetrics_horizontalAdvance(QFontMetricsH handle, PWideString AnonParam1, int len)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	return (int) ((QFontMetrics *)handle)->horizontalAdvance(t_AnonParam1, len);
}

int QFontMetrics_horizontalAdvance2(QFontMetricsH handle, PWideChar AnonParam1)
{
	return (int) ((QFontMetrics *)handle)->horizontalAdvance(*(QChar *)AnonParam1);
}

void QFontMetrics_boundingRect(QFontMetricsH handle, PRect retval, PWideChar AnonParam1)
{
	QRect t_retval;
	t_retval = ((QFontMetrics *)handle)->boundingRect(*(QChar *)AnonParam1);
	copyQRectToPRect(t_retval, retval);
}

void QFontMetrics_boundingRect2(QFontMetricsH handle, PRect retval, PWideString text)
{
	QRect t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QFontMetrics *)handle)->boundingRect(t_text);
	copyQRectToPRect(t_retval, retval);
}

void QFontMetrics_boundingRect3(QFontMetricsH handle, PRect retval, PRect r, int flags, PWideString text, int tabstops, int* tabarray)
{
	QRect t_retval;
	QRect t_r;
	QString t_text;
	copyPRectToQRect(r, t_r);
	copyPWideStringToQString(text, t_text);
	t_retval = ((QFontMetrics *)handle)->boundingRect(t_r, flags, t_text, tabstops, tabarray);
	copyQRectToPRect(t_retval, retval);
}

void QFontMetrics_boundingRect4(QFontMetricsH handle, PRect retval, int x, int y, int w, int h, int flags, PWideString text, int tabstops, int* tabarray)
{
	QRect t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QFontMetrics *)handle)->boundingRect(x, y, w, h, flags, t_text, tabstops, tabarray);
	copyQRectToPRect(t_retval, retval);
}

void QFontMetrics_size(QFontMetricsH handle, PSize retval, int flags, PWideString str, int tabstops, int* tabarray)
{
	QString t_str;
	copyPWideStringToQString(str, t_str);
	*(QSize *)retval = ((QFontMetrics *)handle)->size(flags, t_str, tabstops, tabarray);
}

void QFontMetrics_tightBoundingRect(QFontMetricsH handle, PRect retval, PWideString text)
{
	QRect t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QFontMetrics *)handle)->tightBoundingRect(t_text);
	copyQRectToPRect(t_retval, retval);
}

void QFontMetrics_elidedText(QFontMetricsH handle, PWideString retval, PWideString text, Qt::TextElideMode mode, int width, int flags)
{
	QString t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QFontMetrics *)handle)->elidedText(t_text, mode, width, flags);
	copyQStringToPWideString(t_retval, retval);
}

int QFontMetrics_horizontalAdvance3(QFontMetricsH handle, const QStringH text, int len)
{
  return (int) ((QFontMetrics *)handle)->horizontalAdvance(*(const QString *)text, len);
}

void QFontMetrics_boundingRect5(QFontMetricsH handle, PRect retval, const QStringH text)
{
	QRect t_retval;
	t_retval = ((QFontMetrics *)handle)->boundingRect(*(const QString*)text);
	copyQRectToPRect(t_retval, retval);
}

void QFontMetrics_boundingRect6(QFontMetricsH handle, PRect retval, PRect r, int flags, const QStringH text, int tabstops, int* tabarray)
{
	QRect t_retval;
	QRect t_r;
	copyPRectToQRect(r, t_r);
	t_retval = ((QFontMetrics *)handle)->boundingRect(t_r, flags, *(const QString *)text, tabstops, tabarray);
	copyQRectToPRect(t_retval, retval);
}

void QFontMetrics_boundingRect7(QFontMetricsH handle, PRect retval, int x, int y, int w, int h, int flags, const QStringH text, int tabstops, int* tabarray)
{
	QRect t_retval;
	t_retval = ((QFontMetrics *)handle)->boundingRect(x, y, w, h, flags, *(const QString*)text, tabstops, tabarray);
	copyQRectToPRect(t_retval, retval);
}

void QFontMetrics_size2(QFontMetricsH handle, PSize retval, int flags, const QStringH str, int tabstops, int* tabarray)
{
	*(QSize *)retval = ((QFontMetrics *)handle)->size(flags, *(const QString *)str, tabstops, tabarray);
}

void QFontMetrics_tightBoundingRect2(QFontMetricsH handle, PRect retval, const QStringH text)
{
	QRect t_retval;
	t_retval = ((QFontMetrics *)handle)->tightBoundingRect(*(const QString*)text);
	copyQRectToPRect(t_retval, retval);
}

void QFontMetrics_elidedText2(QFontMetricsH handle, QStringH retval, QStringH text, Qt::TextElideMode mode, int width, int flags)
{
	*(QString *) retval = ((QFontMetricsF *)handle)->elidedText(*(const QString*)text, mode, width, flags);
}


int QFontMetrics_underlinePos(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->underlinePos();
}

int QFontMetrics_overlinePos(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->overlinePos();
}

int QFontMetrics_strikeOutPos(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->strikeOutPos();
}

int QFontMetrics_lineWidth(QFontMetricsH handle)
{
	return (int) ((QFontMetrics *)handle)->lineWidth();
}

QFontMetricsFH QFontMetricsF_Create(const QFontH AnonParam1)
{
	return (QFontMetricsFH) new QFontMetricsF(*(const QFont*)AnonParam1);
}

void QFontMetricsF_Destroy(QFontMetricsFH handle)
{
	delete (QFontMetricsF *)handle;
}

QFontMetricsFH QFontMetricsF_Create2(const QFontH AnonParam1, QPaintDeviceH pd)
{
	return (QFontMetricsFH) new QFontMetricsF(*(const QFont*)AnonParam1, (QPaintDevice*)pd);
}

QFontMetricsFH QFontMetricsF_Create4(const QFontMetricsFH AnonParam1)
{
	return (QFontMetricsFH) new QFontMetricsF(*(const QFontMetricsF*)AnonParam1);
}

void QFontMetricsF_swap(QFontMetricsFH handle, QFontMetricsFH other)
{
	((QFontMetricsF *)handle)->swap(*(QFontMetricsF*)other);
}

qreal QFontMetricsF_ascent(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->ascent();
}

qreal QFontMetricsF_capHeight(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->capHeight();
}

qreal QFontMetricsF_descent(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->descent();
}

qreal QFontMetricsF_fontDpi(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->fontDpi();
}

qreal QFontMetricsF_height(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->height();
}

qreal QFontMetricsF_leading(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->leading();
}

qreal QFontMetricsF_lineSpacing(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->lineSpacing();
}

qreal QFontMetricsF_minLeftBearing(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->minLeftBearing();
}

qreal QFontMetricsF_minRightBearing(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->minRightBearing();
}

qreal QFontMetricsF_maxWidth(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->maxWidth();
}

qreal QFontMetricsF_xHeight(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->xHeight();
}

qreal QFontMetricsF_averageCharWidth(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->averageCharWidth();
}

bool QFontMetricsF_inFont(QFontMetricsFH handle, PWideChar AnonParam1)
{
	return (bool) ((QFontMetricsF *)handle)->inFont(*(QChar *)AnonParam1);
}

bool QFontMetricsF_inFontUcs4(QFontMetricsFH handle, uint ucs4)
{
	return (bool) ((QFontMetricsF *)handle)->inFontUcs4(ucs4);
}

qreal QFontMetricsF_leftBearing(QFontMetricsFH handle, PWideChar AnonParam1)
{
	return (qreal) ((QFontMetricsF *)handle)->leftBearing(*(QChar *)AnonParam1);
}

qreal QFontMetricsF_rightBearing(QFontMetricsFH handle, PWideChar AnonParam1)
{
	return (qreal) ((QFontMetricsF *)handle)->rightBearing(*(QChar *)AnonParam1);
}

qreal QFontMetricsF_horizontalAdvance(QFontMetricsFH handle, PWideString AnonParam1, int len)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	return (qreal) ((QFontMetricsF *)handle)->horizontalAdvance(t_AnonParam1, len);
}

qreal QFontMetricsF_horizontalAdvance2(QFontMetricsFH handle, PWideChar AnonParam1)
{
	return (qreal) ((QFontMetricsF *)handle)->horizontalAdvance(*(QChar *)AnonParam1);
}

qreal QFontMetricsF_horizontalAdvance3(QFontMetricsFH handle, const QStringH text, int len)
{
  return (qreal) ((QFontMetricsF *)handle)->horizontalAdvance(*(const QString *)text, len);
}

void QFontMetricsF_boundingRect(QFontMetricsFH handle, QRectFH retval, PWideString string)
{
	QString t_string;
	copyPWideStringToQString(string, t_string);
	*(QRectF *)retval = ((QFontMetricsF *)handle)->boundingRect(t_string);
}

void QFontMetricsF_boundingRect2(QFontMetricsFH handle, QRectFH retval, PWideChar AnonParam1)
{
	*(QRectF *)retval = ((QFontMetricsF *)handle)->boundingRect(*(QChar *)AnonParam1);
}

void QFontMetricsF_boundingRect3(QFontMetricsFH handle, QRectFH retval, const QRectFH r, int flags, PWideString string, int tabstops, int* tabarray)
{
	QString t_string;
	copyPWideStringToQString(string, t_string);
	*(QRectF *)retval = ((QFontMetricsF *)handle)->boundingRect(*(const QRectF*)r, flags, t_string, tabstops, tabarray);
}

void QFontMetricsF_size(QFontMetricsFH handle, QSizeFH retval, int flags, PWideString str, int tabstops, int* tabarray)
{
	QString t_str;
	copyPWideStringToQString(str, t_str);
	*(QSizeF *)retval = ((QFontMetricsF *)handle)->size(flags, t_str, tabstops, tabarray);
}

void QFontMetricsF_tightBoundingRect(QFontMetricsFH handle, QRectFH retval, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	*(QRectF *)retval = ((QFontMetricsF *)handle)->tightBoundingRect(t_text);
}

void QFontMetricsF_elidedText(QFontMetricsFH handle, PWideString retval, PWideString text, Qt::TextElideMode mode, qreal width, int flags)
{
	QString t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QFontMetricsF *)handle)->elidedText(t_text, mode, width, flags);
	copyQStringToPWideString(t_retval, retval);
}

void QFontMetricsF_boundingRect5(QFontMetricsFH handle, QRectFH retval, const QStringH text)
{
	*(QRectF *) retval = ((QFontMetricsF *)handle)->boundingRect(*(const QString*)text);
}

void QFontMetricsF_boundingRect6(QFontMetricsFH handle, QRectFH retval, QRectFH r, int flags, const QStringH text, int tabstops, int* tabarray)
{
	*(QRectF *)retval = ((QFontMetricsF *)handle)->boundingRect(*(const QRectF*)r, flags, *(const QString *)text, tabstops, tabarray);
}

void QFontMetricsF_size2(QFontMetricsFH handle, QSizeFH retval, int flags, const QStringH str, int tabstops, int* tabarray)
{
	*(QSizeF *)retval = ((QFontMetricsF *)handle)->size(flags, *(const QString *)str, tabstops, tabarray);
}

void QFontMetricsF_tightBoundingRect2(QFontMetricsFH handle, QRectFH retval, const QStringH text)
{
	*(QRectF *)retval = ((QFontMetricsF *)handle)->tightBoundingRect(*(const QString*)text);
}

void QFontMetricsF_elidedText2(QFontMetricsFH handle, QStringH retval, QStringH text, Qt::TextElideMode mode, int width, int flags)
{
	*(QString *) retval = ((QFontMetricsF *)handle)->elidedText(*(const QString*)text, mode, width, flags);
}

qreal QFontMetricsF_underlinePos(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->underlinePos();
}

qreal QFontMetricsF_overlinePos(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->overlinePos();
}

qreal QFontMetricsF_strikeOutPos(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->strikeOutPos();
}

qreal QFontMetricsF_lineWidth(QFontMetricsFH handle)
{
	return (qreal) ((QFontMetricsF *)handle)->lineWidth();
}

