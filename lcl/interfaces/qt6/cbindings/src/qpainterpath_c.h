//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPAINTERPATH_C_H
#define QPAINTERPATH_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QPainterPathH QPainterPath_Create();
C_EXPORT void QPainterPath_Destroy(QPainterPathH handle);
C_EXPORT QPainterPathH QPainterPath_Create2(const QPointFH startPoint);
C_EXPORT QPainterPathH QPainterPath_Create3(const QPainterPathH other);
C_EXPORT void QPainterPath_swap(QPainterPathH handle, QPainterPathH other);
C_EXPORT void QPainterPath_closeSubpath(QPainterPathH handle);
C_EXPORT void QPainterPath_moveTo(QPainterPathH handle, const QPointFH p);
C_EXPORT void QPainterPath_moveTo2(QPainterPathH handle, qreal x, qreal y);
C_EXPORT void QPainterPath_lineTo(QPainterPathH handle, const QPointFH p);
C_EXPORT void QPainterPath_lineTo2(QPainterPathH handle, qreal x, qreal y);
C_EXPORT void QPainterPath_arcMoveTo(QPainterPathH handle, const QRectFH rect, qreal angle);
C_EXPORT void QPainterPath_arcMoveTo2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h, qreal angle);
C_EXPORT void QPainterPath_arcTo(QPainterPathH handle, const QRectFH rect, qreal startAngle, qreal arcLength);
C_EXPORT void QPainterPath_arcTo2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h, qreal startAngle, qreal arcLength);
C_EXPORT void QPainterPath_cubicTo(QPainterPathH handle, const QPointFH ctrlPt1, const QPointFH ctrlPt2, const QPointFH endPt);
C_EXPORT void QPainterPath_cubicTo2(QPainterPathH handle, qreal ctrlPt1x, qreal ctrlPt1y, qreal ctrlPt2x, qreal ctrlPt2y, qreal endPtx, qreal endPty);
C_EXPORT void QPainterPath_quadTo(QPainterPathH handle, const QPointFH ctrlPt, const QPointFH endPt);
C_EXPORT void QPainterPath_quadTo2(QPainterPathH handle, qreal ctrlPtx, qreal ctrlPty, qreal endPtx, qreal endPty);
C_EXPORT void QPainterPath_currentPosition(QPainterPathH handle, PQtPointF retval);
C_EXPORT void QPainterPath_addRect(QPainterPathH handle, const QRectFH rect);
C_EXPORT void QPainterPath_addRect2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h);
C_EXPORT void QPainterPath_addEllipse(QPainterPathH handle, const QRectFH rect);
C_EXPORT void QPainterPath_addEllipse2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h);
C_EXPORT void QPainterPath_addEllipse3(QPainterPathH handle, const QPointFH center, qreal rx, qreal ry);
C_EXPORT void QPainterPath_addPolygon(QPainterPathH handle, const QPolygonFH polygon);
C_EXPORT void QPainterPath_addText(QPainterPathH handle, const QPointFH point, const QFontH f, PWideString text);
C_EXPORT void QPainterPath_addText2(QPainterPathH handle, qreal x, qreal y, const QFontH f, PWideString text);
C_EXPORT void QPainterPath_addPath(QPainterPathH handle, const QPainterPathH path);
C_EXPORT void QPainterPath_addRegion(QPainterPathH handle, const QRegionH region);
C_EXPORT void QPainterPath_addRoundedRect(QPainterPathH handle, const QRectFH rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode);
C_EXPORT void QPainterPath_addRoundedRect2(QPainterPathH handle, qreal x, qreal y, qreal w, qreal h, qreal xRadius, qreal yRadius, Qt::SizeMode mode);
C_EXPORT void QPainterPath_connectPath(QPainterPathH handle, const QPainterPathH path);
C_EXPORT bool QPainterPath_contains(QPainterPathH handle, const QPointFH pt);
C_EXPORT bool QPainterPath_contains2(QPainterPathH handle, const QRectFH rect);
C_EXPORT bool QPainterPath_intersects(QPainterPathH handle, const QRectFH rect);
C_EXPORT void QPainterPath_translate(QPainterPathH handle, qreal dx, qreal dy);
C_EXPORT void QPainterPath_translate2(QPainterPathH handle, const QPointFH offset);
C_EXPORT void QPainterPath_translated(QPainterPathH handle, QPainterPathH retval, qreal dx, qreal dy);
C_EXPORT void QPainterPath_translated2(QPainterPathH handle, QPainterPathH retval, const QPointFH offset);
C_EXPORT void QPainterPath_boundingRect(QPainterPathH handle, QRectFH retval);
C_EXPORT void QPainterPath_controlPointRect(QPainterPathH handle, QRectFH retval);
C_EXPORT Qt::FillRule QPainterPath_fillRule(QPainterPathH handle);
C_EXPORT void QPainterPath_setFillRule(QPainterPathH handle, Qt::FillRule fillRule);
C_EXPORT bool QPainterPath_isEmpty(QPainterPathH handle);
C_EXPORT void QPainterPath_toReversed(QPainterPathH handle, QPainterPathH retval);
C_EXPORT void QPainterPath_toFillPolygon2(QPainterPathH handle, QPolygonFH retval, const QTransformH matrix);
C_EXPORT int QPainterPath_elementCount(QPainterPathH handle);
C_EXPORT void QPainterPath_setElementPositionAt(QPainterPathH handle, int i, qreal x, qreal y);
C_EXPORT qreal QPainterPath_length(QPainterPathH handle);
C_EXPORT qreal QPainterPath_percentAtLength(QPainterPathH handle, qreal t);
C_EXPORT void QPainterPath_pointAtPercent(QPainterPathH handle, PQtPointF retval, qreal t);
C_EXPORT qreal QPainterPath_angleAtPercent(QPainterPathH handle, qreal t);
C_EXPORT qreal QPainterPath_slopeAtPercent(QPainterPathH handle, qreal t);
C_EXPORT bool QPainterPath_intersects2(QPainterPathH handle, const QPainterPathH p);
C_EXPORT bool QPainterPath_contains3(QPainterPathH handle, const QPainterPathH p);
C_EXPORT void QPainterPath_united(QPainterPathH handle, QPainterPathH retval, const QPainterPathH r);
C_EXPORT void QPainterPath_intersected(QPainterPathH handle, QPainterPathH retval, const QPainterPathH r);
C_EXPORT void QPainterPath_subtracted(QPainterPathH handle, QPainterPathH retval, const QPainterPathH r);
C_EXPORT void QPainterPath_simplified(QPainterPathH handle, QPainterPathH retval);
C_EXPORT int QPainterPath_capacity(QPainterPathH handle);
C_EXPORT void QPainterPath_clear(QPainterPathH handle);
C_EXPORT void QPainterPath_reserve(QPainterPathH handle, int size);
C_EXPORT QPainterPathStrokerH QPainterPathStroker_Create();
C_EXPORT void QPainterPathStroker_Destroy(QPainterPathStrokerH handle);
C_EXPORT void QPainterPathStroker_setWidth(QPainterPathStrokerH handle, qreal width);
C_EXPORT qreal QPainterPathStroker_width(QPainterPathStrokerH handle);
C_EXPORT void QPainterPathStroker_setCapStyle(QPainterPathStrokerH handle, Qt::PenCapStyle style);
C_EXPORT Qt::PenCapStyle QPainterPathStroker_capStyle(QPainterPathStrokerH handle);
C_EXPORT void QPainterPathStroker_setJoinStyle(QPainterPathStrokerH handle, Qt::PenJoinStyle style);
C_EXPORT Qt::PenJoinStyle QPainterPathStroker_joinStyle(QPainterPathStrokerH handle);
C_EXPORT void QPainterPathStroker_setMiterLimit(QPainterPathStrokerH handle, qreal length);
C_EXPORT qreal QPainterPathStroker_miterLimit(QPainterPathStrokerH handle);
C_EXPORT void QPainterPathStroker_setCurveThreshold(QPainterPathStrokerH handle, qreal threshold);
C_EXPORT qreal QPainterPathStroker_curveThreshold(QPainterPathStrokerH handle);
C_EXPORT void QPainterPathStroker_setDashPattern(QPainterPathStrokerH handle, Qt::PenStyle AnonParam1);
C_EXPORT void QPainterPathStroker_setDashPattern2(QPainterPathStrokerH handle, PQRealArray dashPattern);
C_EXPORT void QPainterPathStroker_dashPattern(QPainterPathStrokerH handle, PQRealArray retval);
C_EXPORT void QPainterPathStroker_setDashOffset(QPainterPathStrokerH handle, qreal offset);
C_EXPORT qreal QPainterPathStroker_dashOffset(QPainterPathStrokerH handle);
C_EXPORT void QPainterPathStroker_createStroke(QPainterPathStrokerH handle, QPainterPathH retval, const QPainterPathH path);

#endif
