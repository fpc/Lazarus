//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPOLYGON_C_H
#define QPOLYGON_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QPolygonH QPolygon_Create();
C_EXPORT void QPolygon_Destroy(QPolygonH handle);
C_EXPORT QPolygonH QPolygon_Create2(int size);
C_EXPORT QPolygonH QPolygon_Create3(const QPolygonH a);
C_EXPORT QPolygonH QPolygon_Create4(PRect r, bool closed);
C_EXPORT QPolygonH QPolygon_Create5(int nPoints, const int* points);
C_EXPORT void QPolygon_swap(QPolygonH handle, QPolygonH other);
C_EXPORT void QPolygon_translate(QPolygonH handle, int dx, int dy);
C_EXPORT void QPolygon_translate2(QPolygonH handle, const QPointH offset);
C_EXPORT void QPolygon_translated(QPolygonH handle, QPolygonH retval, int dx, int dy);
C_EXPORT void QPolygon_translated2(QPolygonH handle, QPolygonH retval, const QPointH offset);
C_EXPORT void QPolygon_boundingRect(QPolygonH handle, PRect retval);
C_EXPORT void QPolygon_point(QPolygonH handle, int i, int* x, int* y);
C_EXPORT void QPolygon_point2(QPolygonH handle, PQtPoint retval, int i);
C_EXPORT void QPolygon_setPoint(QPolygonH handle, int index, int x, int y);
C_EXPORT void QPolygon_setPoint2(QPolygonH handle, int index, const QPointH p);
C_EXPORT void QPolygon_setPoints(QPolygonH handle, int nPoints, const int* points);
C_EXPORT void QPolygon_putPoints(QPolygonH handle, int index, int nPoints, const int* points);
C_EXPORT void QPolygon_putPoints3(QPolygonH handle, int index, int nPoints, const QPolygonH from, int fromIndex);
C_EXPORT bool QPolygon_containsPoint(QPolygonH handle, const QPointH pt, Qt::FillRule fillRule);
C_EXPORT void QPolygon_united(QPolygonH handle, QPolygonH retval, const QPolygonH r);
C_EXPORT void QPolygon_intersected(QPolygonH handle, QPolygonH retval, const QPolygonH r);
C_EXPORT void QPolygon_subtracted(QPolygonH handle, QPolygonH retval, const QPolygonH r);
C_EXPORT QPolygonFH QPolygonF_Create();
C_EXPORT void QPolygonF_Destroy(QPolygonFH handle);
C_EXPORT QPolygonFH QPolygonF_Create2(int size);
C_EXPORT QPolygonFH QPolygonF_Create3(const QPolygonFH a);
C_EXPORT QPolygonFH QPolygonF_Create4(const QRectFH r);
C_EXPORT QPolygonFH QPolygonF_Create5(const QPolygonH a);
C_EXPORT void QPolygonF_swap(QPolygonFH handle, QPolygonFH other);
C_EXPORT void QPolygonF_translate(QPolygonFH handle, qreal dx, qreal dy);
C_EXPORT void QPolygonF_translate2(QPolygonFH handle, const QPointFH offset);
C_EXPORT void QPolygonF_translated(QPolygonFH handle, QPolygonFH retval, qreal dx, qreal dy);
C_EXPORT void QPolygonF_translated2(QPolygonFH handle, QPolygonFH retval, const QPointFH offset);
C_EXPORT void QPolygonF_toPolygon(QPolygonFH handle, QPolygonH retval);
C_EXPORT bool QPolygonF_isClosed(QPolygonFH handle);
C_EXPORT void QPolygonF_boundingRect(QPolygonFH handle, QRectFH retval);
C_EXPORT bool QPolygonF_containsPoint(QPolygonFH handle, const QPointFH pt, Qt::FillRule fillRule);
C_EXPORT void QPolygonF_united(QPolygonFH handle, QPolygonFH retval, const QPolygonFH r);
C_EXPORT void QPolygonF_intersected(QPolygonFH handle, QPolygonFH retval, const QPolygonFH r);
C_EXPORT void QPolygonF_subtracted(QPolygonFH handle, QPolygonFH retval, const QPolygonFH r);

#endif
