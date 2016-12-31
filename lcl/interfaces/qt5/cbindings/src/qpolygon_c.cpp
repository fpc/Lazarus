//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpolygon_c.h"

QPolygonH QPolygon_Create()
{
	return (QPolygonH) new QPolygon();
}

void QPolygon_Destroy(QPolygonH handle)
{
	delete (QPolygon *)handle;
}

QPolygonH QPolygon_Create2(int size)
{
	return (QPolygonH) new QPolygon(size);
}

QPolygonH QPolygon_Create3(const QPolygonH a)
{
	return (QPolygonH) new QPolygon(*(const QPolygon*)a);
}

QPolygonH QPolygon_Create4(PRect r, bool closed)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	return (QPolygonH) new QPolygon(t_r, closed);
}

QPolygonH QPolygon_Create5(int nPoints, const int* points)
{
	return (QPolygonH) new QPolygon(nPoints, points);
}

void QPolygon_swap(QPolygonH handle, QPolygonH other)
{
	((QPolygon *)handle)->swap(*(QPolygon*)other);
}

void QPolygon_translate(QPolygonH handle, int dx, int dy)
{
	((QPolygon *)handle)->translate(dx, dy);
}

void QPolygon_translate2(QPolygonH handle, const QPointH offset)
{
	((QPolygon *)handle)->translate(*(const QPoint*)offset);
}

void QPolygon_translated(QPolygonH handle, QPolygonH retval, int dx, int dy)
{
	*(QPolygon *)retval = ((QPolygon *)handle)->translated(dx, dy);
}

void QPolygon_translated2(QPolygonH handle, QPolygonH retval, const QPointH offset)
{
	*(QPolygon *)retval = ((QPolygon *)handle)->translated(*(const QPoint*)offset);
}

void QPolygon_boundingRect(QPolygonH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPolygon *)handle)->boundingRect();
	copyQRectToPRect(t_retval, retval);
}

void QPolygon_point(QPolygonH handle, int i, int* x, int* y)
{
	((QPolygon *)handle)->point(i, x, y);
}

void QPolygon_point2(QPolygonH handle, PQtPoint retval, int i)
{
	*(QPoint *)retval = ((QPolygon *)handle)->point(i);
}

void QPolygon_setPoint(QPolygonH handle, int index, int x, int y)
{
	((QPolygon *)handle)->setPoint(index, x, y);
}

void QPolygon_setPoint2(QPolygonH handle, int index, const QPointH p)
{
	((QPolygon *)handle)->setPoint(index, *(const QPoint*)p);
}

void QPolygon_setPoints(QPolygonH handle, int nPoints, const int* points)
{
	((QPolygon *)handle)->setPoints(nPoints, points);
}

void QPolygon_putPoints(QPolygonH handle, int index, int nPoints, const int* points)
{
	((QPolygon *)handle)->putPoints(index, nPoints, points);
}

void QPolygon_putPoints3(QPolygonH handle, int index, int nPoints, const QPolygonH from, int fromIndex)
{
	((QPolygon *)handle)->putPoints(index, nPoints, *(const QPolygon*)from, fromIndex);
}

bool QPolygon_containsPoint(QPolygonH handle, const QPointH pt, Qt::FillRule fillRule)
{
	return (bool) ((QPolygon *)handle)->containsPoint(*(const QPoint*)pt, fillRule);
}

void QPolygon_united(QPolygonH handle, QPolygonH retval, const QPolygonH r)
{
	*(QPolygon *)retval = ((QPolygon *)handle)->united(*(const QPolygon*)r);
}

void QPolygon_intersected(QPolygonH handle, QPolygonH retval, const QPolygonH r)
{
	*(QPolygon *)retval = ((QPolygon *)handle)->intersected(*(const QPolygon*)r);
}

void QPolygon_subtracted(QPolygonH handle, QPolygonH retval, const QPolygonH r)
{
	*(QPolygon *)retval = ((QPolygon *)handle)->subtracted(*(const QPolygon*)r);
}

QPolygonFH QPolygonF_Create()
{
	return (QPolygonFH) new QPolygonF();
}

void QPolygonF_Destroy(QPolygonFH handle)
{
	delete (QPolygonF *)handle;
}

QPolygonFH QPolygonF_Create2(int size)
{
	return (QPolygonFH) new QPolygonF(size);
}

QPolygonFH QPolygonF_Create3(const QPolygonFH a)
{
	return (QPolygonFH) new QPolygonF(*(const QPolygonF*)a);
}

QPolygonFH QPolygonF_Create4(const QRectFH r)
{
	return (QPolygonFH) new QPolygonF(*(const QRectF*)r);
}

QPolygonFH QPolygonF_Create5(const QPolygonH a)
{
	return (QPolygonFH) new QPolygonF(*(const QPolygon*)a);
}

void QPolygonF_swap(QPolygonFH handle, QPolygonFH other)
{
	((QPolygonF *)handle)->swap(*(QPolygonF*)other);
}

void QPolygonF_translate(QPolygonFH handle, qreal dx, qreal dy)
{
	((QPolygonF *)handle)->translate(dx, dy);
}

void QPolygonF_translate2(QPolygonFH handle, const QPointFH offset)
{
	((QPolygonF *)handle)->translate(*(const QPointF*)offset);
}

void QPolygonF_translated(QPolygonFH handle, QPolygonFH retval, qreal dx, qreal dy)
{
	*(QPolygonF *)retval = ((QPolygonF *)handle)->translated(dx, dy);
}

void QPolygonF_translated2(QPolygonFH handle, QPolygonFH retval, const QPointFH offset)
{
	*(QPolygonF *)retval = ((QPolygonF *)handle)->translated(*(const QPointF*)offset);
}

void QPolygonF_toPolygon(QPolygonFH handle, QPolygonH retval)
{
	*(QPolygon *)retval = ((QPolygonF *)handle)->toPolygon();
}

bool QPolygonF_isClosed(QPolygonFH handle)
{
	return (bool) ((QPolygonF *)handle)->isClosed();
}

void QPolygonF_boundingRect(QPolygonFH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QPolygonF *)handle)->boundingRect();
}

bool QPolygonF_containsPoint(QPolygonFH handle, const QPointFH pt, Qt::FillRule fillRule)
{
	return (bool) ((QPolygonF *)handle)->containsPoint(*(const QPointF*)pt, fillRule);
}

void QPolygonF_united(QPolygonFH handle, QPolygonFH retval, const QPolygonFH r)
{
	*(QPolygonF *)retval = ((QPolygonF *)handle)->united(*(const QPolygonF*)r);
}

void QPolygonF_intersected(QPolygonFH handle, QPolygonFH retval, const QPolygonFH r)
{
	*(QPolygonF *)retval = ((QPolygonF *)handle)->intersected(*(const QPolygonF*)r);
}

void QPolygonF_subtracted(QPolygonFH handle, QPolygonFH retval, const QPolygonFH r)
{
	*(QPolygonF *)retval = ((QPolygonF *)handle)->subtracted(*(const QPolygonF*)r);
}

