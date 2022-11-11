//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpainterpath_c.h"

QPainterPathH QPainterPath_Create()
{
	return (QPainterPathH) new QPainterPath();
}

void QPainterPath_Destroy(QPainterPathH handle)
{
	delete (QPainterPath *)handle;
}

QPainterPathH QPainterPath_Create2(const QPointFH startPoint)
{
	return (QPainterPathH) new QPainterPath(*(const QPointF*)startPoint);
}

QPainterPathH QPainterPath_Create3(const QPainterPathH other)
{
	return (QPainterPathH) new QPainterPath(*(const QPainterPath*)other);
}

void QPainterPath_swap(QPainterPathH handle, QPainterPathH other)
{
	((QPainterPath *)handle)->swap(*(QPainterPath*)other);
}

void QPainterPath_closeSubpath(QPainterPathH handle)
{
	((QPainterPath *)handle)->closeSubpath();
}

void QPainterPath_moveTo(QPainterPathH handle, const QPointFH p)
{
	((QPainterPath *)handle)->moveTo(*(const QPointF*)p);
}

void QPainterPath_moveTo2(QPainterPathH handle, qreal x, qreal y)
{
	((QPainterPath *)handle)->moveTo(x, y);
}

void QPainterPath_lineTo(QPainterPathH handle, const QPointFH p)
{
	((QPainterPath *)handle)->lineTo(*(const QPointF*)p);
}

void QPainterPath_lineTo2(QPainterPathH handle, qreal x, qreal y)
{
	((QPainterPath *)handle)->lineTo(x, y);
}

void QPainterPath_arcMoveTo(QPainterPathH handle, const QRectFH rect, qreal angle)
{
	((QPainterPath *)handle)->arcMoveTo(*(const QRectF*)rect, angle);
}

void QPainterPath_arcMoveTo2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h, qreal angle)
{
	((QPainterPath *)handle)->arcMoveTo(x, y, w, h, angle);
}

void QPainterPath_arcTo(QPainterPathH handle, const QRectFH rect, qreal startAngle, qreal arcLength)
{
	((QPainterPath *)handle)->arcTo(*(const QRectF*)rect, startAngle, arcLength);
}

void QPainterPath_arcTo2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h, qreal startAngle, qreal arcLength)
{
	((QPainterPath *)handle)->arcTo(x, y, w, h, startAngle, arcLength);
}

void QPainterPath_cubicTo(QPainterPathH handle, const QPointFH ctrlPt1, const QPointFH ctrlPt2, const QPointFH endPt)
{
	((QPainterPath *)handle)->cubicTo(*(const QPointF*)ctrlPt1, *(const QPointF*)ctrlPt2, *(const QPointF*)endPt);
}

void QPainterPath_cubicTo2(QPainterPathH handle, qreal ctrlPt1x, qreal ctrlPt1y, qreal ctrlPt2x, qreal ctrlPt2y, qreal endPtx, qreal endPty)
{
	((QPainterPath *)handle)->cubicTo(ctrlPt1x, ctrlPt1y, ctrlPt2x, ctrlPt2y, endPtx, endPty);
}

void QPainterPath_quadTo(QPainterPathH handle, const QPointFH ctrlPt, const QPointFH endPt)
{
	((QPainterPath *)handle)->quadTo(*(const QPointF*)ctrlPt, *(const QPointF*)endPt);
}

void QPainterPath_quadTo2(QPainterPathH handle, qreal ctrlPtx, qreal ctrlPty, qreal endPtx, qreal endPty)
{
	((QPainterPath *)handle)->quadTo(ctrlPtx, ctrlPty, endPtx, endPty);
}

void QPainterPath_currentPosition(QPainterPathH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QPainterPath *)handle)->currentPosition();
}

void QPainterPath_addRect(QPainterPathH handle, const QRectFH rect)
{
	((QPainterPath *)handle)->addRect(*(const QRectF*)rect);
}

void QPainterPath_addRect2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h)
{
	((QPainterPath *)handle)->addRect(x, y, w, h);
}

void QPainterPath_addEllipse(QPainterPathH handle, const QRectFH rect)
{
	((QPainterPath *)handle)->addEllipse(*(const QRectF*)rect);
}

void QPainterPath_addEllipse2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h)
{
	((QPainterPath *)handle)->addEllipse(x, y, w, h);
}

void QPainterPath_addEllipse3(QPainterPathH handle, const QPointFH center, qreal rx, qreal ry)
{
	((QPainterPath *)handle)->addEllipse(*(const QPointF*)center, rx, ry);
}

void QPainterPath_addPolygon(QPainterPathH handle, const QPolygonFH polygon)
{
	((QPainterPath *)handle)->addPolygon(*(const QPolygonF*)polygon);
}

void QPainterPath_addText(QPainterPathH handle, const QPointFH point, const QFontH f, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QPainterPath *)handle)->addText(*(const QPointF*)point, *(const QFont*)f, t_text);
}

void QPainterPath_addText2(QPainterPathH handle, qreal x, qreal y, const QFontH f, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QPainterPath *)handle)->addText(x, y, *(const QFont*)f, t_text);
}

void QPainterPath_addPath(QPainterPathH handle, const QPainterPathH path)
{
	((QPainterPath *)handle)->addPath(*(const QPainterPath*)path);
}

void QPainterPath_addRegion(QPainterPathH handle, const QRegionH region)
{
	((QPainterPath *)handle)->addRegion(*(const QRegion*)region);
}

void QPainterPath_addRoundedRect(QPainterPathH handle, const QRectFH rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode)
{
	((QPainterPath *)handle)->addRoundedRect(*(const QRectF*)rect, xRadius, yRadius, mode);
}

void QPainterPath_addRoundedRect2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h, qreal xRadius, qreal yRadius, Qt::SizeMode mode)
{
	((QPainterPath *)handle)->addRoundedRect(x, y, w, h, xRadius, yRadius, mode);
}

void QPainterPath_connectPath(QPainterPathH handle, const QPainterPathH path)
{
	((QPainterPath *)handle)->connectPath(*(const QPainterPath*)path);
}

bool QPainterPath_contains(QPainterPathH handle, const QPointFH pt)
{
	return (bool) ((QPainterPath *)handle)->contains(*(const QPointF*)pt);
}

bool QPainterPath_contains2(QPainterPathH handle, const QRectFH rect)
{
	return (bool) ((QPainterPath *)handle)->contains(*(const QRectF*)rect);
}

bool QPainterPath_intersects(QPainterPathH handle, const QRectFH rect)
{
	return (bool) ((QPainterPath *)handle)->intersects(*(const QRectF*)rect);
}

void QPainterPath_translate(QPainterPathH handle, qreal dx, qreal dy)
{
	((QPainterPath *)handle)->translate(dx, dy);
}

void QPainterPath_translate2(QPainterPathH handle, const QPointFH offset)
{
	((QPainterPath *)handle)->translate(*(const QPointF*)offset);
}

void QPainterPath_translated(QPainterPathH handle, QPainterPathH retval, qreal dx, qreal dy)
{
	*(QPainterPath *)retval = ((QPainterPath *)handle)->translated(dx, dy);
}

void QPainterPath_translated2(QPainterPathH handle, QPainterPathH retval, const QPointFH offset)
{
	*(QPainterPath *)retval = ((QPainterPath *)handle)->translated(*(const QPointF*)offset);
}

void QPainterPath_boundingRect(QPainterPathH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QPainterPath *)handle)->boundingRect();
}

void QPainterPath_controlPointRect(QPainterPathH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QPainterPath *)handle)->controlPointRect();
}

Qt::FillRule QPainterPath_fillRule(QPainterPathH handle)
{
	return (Qt::FillRule) ((QPainterPath *)handle)->fillRule();
}

void QPainterPath_setFillRule(QPainterPathH handle, Qt::FillRule fillRule)
{
	((QPainterPath *)handle)->setFillRule(fillRule);
}

bool QPainterPath_isEmpty(QPainterPathH handle)
{
	return (bool) ((QPainterPath *)handle)->isEmpty();
}

void QPainterPath_toReversed(QPainterPathH handle, QPainterPathH retval)
{
	*(QPainterPath *)retval = ((QPainterPath *)handle)->toReversed();
}

void QPainterPath_toFillPolygon2(QPainterPathH handle, QPolygonFH retval, const QTransformH matrix)
{
	*(QPolygonF *)retval = ((QPainterPath *)handle)->toFillPolygon(*(const QTransform*)matrix);
}

int QPainterPath_elementCount(QPainterPathH handle)
{
	return (int) ((QPainterPath *)handle)->elementCount();
}

void QPainterPath_setElementPositionAt(QPainterPathH handle, int i, qreal x, qreal y)
{
	((QPainterPath *)handle)->setElementPositionAt(i, x, y);
}

qreal QPainterPath_length(QPainterPathH handle)
{
	return (qreal) ((QPainterPath *)handle)->length();
}

qreal QPainterPath_percentAtLength(QPainterPathH handle, qreal t)
{
	return (qreal) ((QPainterPath *)handle)->percentAtLength(t);
}

void QPainterPath_pointAtPercent(QPainterPathH handle, PQtPointF retval, qreal t)
{
	*(QPointF *)retval = ((QPainterPath *)handle)->pointAtPercent(t);
}

qreal QPainterPath_angleAtPercent(QPainterPathH handle, qreal t)
{
	return (qreal) ((QPainterPath *)handle)->angleAtPercent(t);
}

qreal QPainterPath_slopeAtPercent(QPainterPathH handle, qreal t)
{
	return (qreal) ((QPainterPath *)handle)->slopeAtPercent(t);
}

bool QPainterPath_intersects2(QPainterPathH handle, const QPainterPathH p)
{
	return (bool) ((QPainterPath *)handle)->intersects(*(const QPainterPath*)p);
}

bool QPainterPath_contains3(QPainterPathH handle, const QPainterPathH p)
{
	return (bool) ((QPainterPath *)handle)->contains(*(const QPainterPath*)p);
}

void QPainterPath_united(QPainterPathH handle, QPainterPathH retval, const QPainterPathH r)
{
	*(QPainterPath *)retval = ((QPainterPath *)handle)->united(*(const QPainterPath*)r);
}

void QPainterPath_intersected(QPainterPathH handle, QPainterPathH retval, const QPainterPathH r)
{
	*(QPainterPath *)retval = ((QPainterPath *)handle)->intersected(*(const QPainterPath*)r);
}

void QPainterPath_subtracted(QPainterPathH handle, QPainterPathH retval, const QPainterPathH r)
{
	*(QPainterPath *)retval = ((QPainterPath *)handle)->subtracted(*(const QPainterPath*)r);
}

void QPainterPath_simplified(QPainterPathH handle, QPainterPathH retval)
{
	*(QPainterPath *)retval = ((QPainterPath *)handle)->simplified();
}

int QPainterPath_capacity(QPainterPathH handle)
{
  return (int) ((QPainterPath *)handle)->capacity();
}

void QPainterPath_clear(QPainterPathH handle)
{
  ((QPainterPath *)handle)->clear();
}

void QPainterPath_reserve(QPainterPathH handle, int size)
{
  ((QPainterPath *)handle)->reserve(size);
}

QPainterPathStrokerH QPainterPathStroker_Create()
{
	return (QPainterPathStrokerH) new QPainterPathStroker();
}

void QPainterPathStroker_Destroy(QPainterPathStrokerH handle)
{
	delete (QPainterPathStroker *)handle;
}

void QPainterPathStroker_setWidth(QPainterPathStrokerH handle, qreal width)
{
	((QPainterPathStroker *)handle)->setWidth(width);
}

qreal QPainterPathStroker_width(QPainterPathStrokerH handle)
{
	return (qreal) ((QPainterPathStroker *)handle)->width();
}

void QPainterPathStroker_setCapStyle(QPainterPathStrokerH handle, Qt::PenCapStyle style)
{
	((QPainterPathStroker *)handle)->setCapStyle(style);
}

Qt::PenCapStyle QPainterPathStroker_capStyle(QPainterPathStrokerH handle)
{
	return (Qt::PenCapStyle) ((QPainterPathStroker *)handle)->capStyle();
}

void QPainterPathStroker_setJoinStyle(QPainterPathStrokerH handle, Qt::PenJoinStyle style)
{
	((QPainterPathStroker *)handle)->setJoinStyle(style);
}

Qt::PenJoinStyle QPainterPathStroker_joinStyle(QPainterPathStrokerH handle)
{
	return (Qt::PenJoinStyle) ((QPainterPathStroker *)handle)->joinStyle();
}

void QPainterPathStroker_setMiterLimit(QPainterPathStrokerH handle, qreal length)
{
	((QPainterPathStroker *)handle)->setMiterLimit(length);
}

qreal QPainterPathStroker_miterLimit(QPainterPathStrokerH handle)
{
	return (qreal) ((QPainterPathStroker *)handle)->miterLimit();
}

void QPainterPathStroker_setCurveThreshold(QPainterPathStrokerH handle, qreal threshold)
{
	((QPainterPathStroker *)handle)->setCurveThreshold(threshold);
}

qreal QPainterPathStroker_curveThreshold(QPainterPathStrokerH handle)
{
	return (qreal) ((QPainterPathStroker *)handle)->curveThreshold();
}

void QPainterPathStroker_setDashPattern(QPainterPathStrokerH handle, Qt::PenStyle AnonParam1)
{
	((QPainterPathStroker *)handle)->setDashPattern(AnonParam1);
}

void QPainterPathStroker_setDashPattern2(QPainterPathStrokerH handle, PQRealArray dashPattern)
{
	QVector<qreal> t_dashPattern;
	copyQRealArrayToQVectorQReal(dashPattern, t_dashPattern);
	((QPainterPathStroker *)handle)->setDashPattern(t_dashPattern);
}

void QPainterPathStroker_dashPattern(QPainterPathStrokerH handle, PQRealArray retval)
{
	QVector<qreal> t_retval;
	t_retval = ((QPainterPathStroker *)handle)->dashPattern();
	copyQVectorQRealToQRealArray(t_retval, retval);
}

void QPainterPathStroker_setDashOffset(QPainterPathStrokerH handle, qreal offset)
{
	((QPainterPathStroker *)handle)->setDashOffset(offset);
}

qreal QPainterPathStroker_dashOffset(QPainterPathStrokerH handle)
{
	return (qreal) ((QPainterPathStroker *)handle)->dashOffset();
}

void QPainterPathStroker_createStroke(QPainterPathStrokerH handle, QPainterPathH retval, const QPainterPathH path)
{
	*(QPainterPath *)retval = ((QPainterPathStroker *)handle)->createStroke(*(const QPainterPath*)path);
}

