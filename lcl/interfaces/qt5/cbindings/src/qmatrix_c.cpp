//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmatrix_c.h"

QMatrixH QMatrix_Create(Qt::Initialization AnonParam1)
{
	return (QMatrixH) new QMatrix(AnonParam1);
}

void QMatrix_Destroy(QMatrixH handle)
{
	delete (QMatrix *)handle;
}

QMatrixH QMatrix_Create2()
{
	return (QMatrixH) new QMatrix();
}

QMatrixH QMatrix_Create3(qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy)
{
	return (QMatrixH) new QMatrix(m11, m12, m21, m22, dx, dy);
}

QMatrixH QMatrix_Create4(const QMatrixH matrix)
{
	return (QMatrixH) new QMatrix(*(const QMatrix*)matrix);
}

void QMatrix_setMatrix(QMatrixH handle, qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy)
{
	((QMatrix *)handle)->setMatrix(m11, m12, m21, m22, dx, dy);
}

qreal QMatrix_m11(QMatrixH handle)
{
	return (qreal) ((QMatrix *)handle)->m11();
}

qreal QMatrix_m12(QMatrixH handle)
{
	return (qreal) ((QMatrix *)handle)->m12();
}

qreal QMatrix_m21(QMatrixH handle)
{
	return (qreal) ((QMatrix *)handle)->m21();
}

qreal QMatrix_m22(QMatrixH handle)
{
	return (qreal) ((QMatrix *)handle)->m22();
}

qreal QMatrix_dx(QMatrixH handle)
{
	return (qreal) ((QMatrix *)handle)->dx();
}

qreal QMatrix_dy(QMatrixH handle)
{
	return (qreal) ((QMatrix *)handle)->dy();
}

void QMatrix_map(QMatrixH handle, int x, int y, int* tx, int* ty)
{
	((QMatrix *)handle)->map(x, y, tx, ty);
}

void QMatrix_map2(QMatrixH handle, qreal x, qreal y, qreal* tx, qreal* ty)
{
	((QMatrix *)handle)->map(x, y, tx, ty);
}

void QMatrix_mapRect(QMatrixH handle, PRect retval, PRect AnonParam1)
{
	QRect t_retval;
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	t_retval = ((QMatrix *)handle)->mapRect(t_AnonParam1);
	copyQRectToPRect(t_retval, retval);
}

void QMatrix_mapRect2(QMatrixH handle, QRectFH retval, const QRectFH AnonParam1)
{
	*(QRectF *)retval = ((QMatrix *)handle)->mapRect(*(const QRectF*)AnonParam1);
}

void QMatrix_map3(QMatrixH handle, PQtPoint retval, const QPointH p)
{
	*(QPoint *)retval = ((QMatrix *)handle)->map(*(const QPoint*)p);
}

void QMatrix_map4(QMatrixH handle, PQtPointF retval, const QPointFH p)
{
	*(QPointF *)retval = ((QMatrix *)handle)->map(*(const QPointF*)p);
}

void QMatrix_map5(QMatrixH handle, QLineH retval, const QLineH l)
{
	*(QLine *)retval = ((QMatrix *)handle)->map(*(const QLine*)l);
}

void QMatrix_map6(QMatrixH handle, QLineFH retval, const QLineFH l)
{
	*(QLineF *)retval = ((QMatrix *)handle)->map(*(const QLineF*)l);
}

void QMatrix_map7(QMatrixH handle, QPolygonFH retval, const QPolygonFH a)
{
	*(QPolygonF *)retval = ((QMatrix *)handle)->map(*(const QPolygonF*)a);
}

void QMatrix_map8(QMatrixH handle, QPolygonH retval, const QPolygonH a)
{
	*(QPolygon *)retval = ((QMatrix *)handle)->map(*(const QPolygon*)a);
}

void QMatrix_map9(QMatrixH handle, QRegionH retval, const QRegionH r)
{
	*(QRegion *)retval = ((QMatrix *)handle)->map(*(const QRegion*)r);
}

void QMatrix_map10(QMatrixH handle, QPainterPathH retval, const QPainterPathH p)
{
	*(QPainterPath *)retval = ((QMatrix *)handle)->map(*(const QPainterPath*)p);
}

void QMatrix_mapToPolygon(QMatrixH handle, QPolygonH retval, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	*(QPolygon *)retval = ((QMatrix *)handle)->mapToPolygon(t_r);
}

void QMatrix_reset(QMatrixH handle)
{
	((QMatrix *)handle)->reset();
}

bool QMatrix_isIdentity(QMatrixH handle)
{
	return (bool) ((QMatrix *)handle)->isIdentity();
}

QMatrixH QMatrix_translate(QMatrixH handle, qreal dx, qreal dy)
{
	return (QMatrixH) &((QMatrix *)handle)->translate(dx, dy);
}

QMatrixH QMatrix_scale(QMatrixH handle, qreal sx, qreal sy)
{
	return (QMatrixH) &((QMatrix *)handle)->scale(sx, sy);
}

QMatrixH QMatrix_shear(QMatrixH handle, qreal sh, qreal sv)
{
	return (QMatrixH) &((QMatrix *)handle)->shear(sh, sv);
}

QMatrixH QMatrix_rotate(QMatrixH handle, qreal a)
{
	return (QMatrixH) &((QMatrix *)handle)->rotate(a);
}

bool QMatrix_isInvertible(QMatrixH handle)
{
	return (bool) ((QMatrix *)handle)->isInvertible();
}

qreal QMatrix_determinant(QMatrixH handle)
{
	return (qreal) ((QMatrix *)handle)->determinant();
}

void QMatrix_inverted(QMatrixH handle, QMatrixH retval, bool* invertible)
{
	*(QMatrix *)retval = ((QMatrix *)handle)->inverted(invertible);
}

