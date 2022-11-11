//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtransform_c.h"

QTransformH QTransform_Create(Qt::Initialization AnonParam1)
{
	return (QTransformH) new QTransform(AnonParam1);
}

void QTransform_Destroy(QTransformH handle)
{
	delete (QTransform *)handle;
}

QTransformH QTransform_Create2()
{
	return (QTransformH) new QTransform();
}

QTransformH QTransform_Create3(qreal h11, qreal h12, qreal h13, qreal h21, qreal h22, qreal h23, qreal h31, qreal h32, qreal h33)
{
	return (QTransformH) new QTransform(h11, h12, h13, h21, h22, h23, h31, h32, h33);
}

QTransformH QTransform_Create4(qreal h11, qreal h12, qreal h21, qreal h22, qreal dx, qreal dy)
{
	return (QTransformH) new QTransform(h11, h12, h21, h22, dx, dy);
}

bool QTransform_isAffine(QTransformH handle)
{
	return (bool) ((QTransform *)handle)->isAffine();
}

bool QTransform_isIdentity(QTransformH handle)
{
	return (bool) ((QTransform *)handle)->isIdentity();
}

bool QTransform_isInvertible(QTransformH handle)
{
	return (bool) ((QTransform *)handle)->isInvertible();
}

bool QTransform_isScaling(QTransformH handle)
{
	return (bool) ((QTransform *)handle)->isScaling();
}

bool QTransform_isRotating(QTransformH handle)
{
	return (bool) ((QTransform *)handle)->isRotating();
}

bool QTransform_isTranslating(QTransformH handle)
{
	return (bool) ((QTransform *)handle)->isTranslating();
}

QTransform::TransformationType QTransform_type(QTransformH handle)
{
	return (QTransform::TransformationType) ((QTransform *)handle)->type();
}

qreal QTransform_determinant(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->determinant();
}

qreal QTransform_m11(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->m11();
}

qreal QTransform_m12(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->m12();
}

qreal QTransform_m13(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->m13();
}

qreal QTransform_m21(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->m21();
}

qreal QTransform_m22(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->m22();
}

qreal QTransform_m23(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->m23();
}

qreal QTransform_m31(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->m31();
}

qreal QTransform_m32(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->m32();
}

qreal QTransform_m33(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->m33();
}

qreal QTransform_dx(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->dx();
}

qreal QTransform_dy(QTransformH handle)
{
	return (qreal) ((QTransform *)handle)->dy();
}

void QTransform_setMatrix(QTransformH handle, qreal m11, qreal m12, qreal m13, qreal m21, qreal m22, qreal m23, qreal m31, qreal m32, qreal m33)
{
	((QTransform *)handle)->setMatrix(m11, m12, m13, m21, m22, m23, m31, m32, m33);
}

void QTransform_inverted(QTransformH handle, QTransformH retval, bool* invertible)
{
	*(QTransform *)retval = ((QTransform *)handle)->inverted(invertible);
}

void QTransform_adjoint(QTransformH handle, QTransformH retval)
{
	*(QTransform *)retval = ((QTransform *)handle)->adjoint();
}

void QTransform_transposed(QTransformH handle, QTransformH retval)
{
	*(QTransform *)retval = ((QTransform *)handle)->transposed();
}

QTransformH QTransform_translate(QTransformH handle, qreal dx, qreal dy)
{
	return (QTransformH) &((QTransform *)handle)->translate(dx, dy);
}

QTransformH QTransform_scale(QTransformH handle, qreal sx, qreal sy)
{
	return (QTransformH) &((QTransform *)handle)->scale(sx, sy);
}

QTransformH QTransform_shear(QTransformH handle, qreal sh, qreal sv)
{
	return (QTransformH) &((QTransform *)handle)->shear(sh, sv);
}

QTransformH QTransform_rotate(QTransformH handle, qreal a, Qt::Axis axis)
{
	return (QTransformH) &((QTransform *)handle)->rotate(a, axis);
}

QTransformH QTransform_rotateRadians(QTransformH handle, qreal a, Qt::Axis axis)
{
	return (QTransformH) &((QTransform *)handle)->rotateRadians(a, axis);
}

bool QTransform_squareToQuad(const QPolygonFH square, QTransformH result)
{
	return (bool) QTransform::squareToQuad(*(const QPolygonF*)square, *(QTransform*)result);
}

bool QTransform_quadToSquare(const QPolygonFH quad, QTransformH result)
{
	return (bool) QTransform::quadToSquare(*(const QPolygonF*)quad, *(QTransform*)result);
}

bool QTransform_quadToQuad(const QPolygonFH one, const QPolygonFH two, QTransformH result)
{
	return (bool) QTransform::quadToQuad(*(const QPolygonF*)one, *(const QPolygonF*)two, *(QTransform*)result);
}

void QTransform_reset(QTransformH handle)
{
	((QTransform *)handle)->reset();
}

void QTransform_map(QTransformH handle, PQtPoint retval, const QPointH p)
{
	*(QPoint *)retval = ((QTransform *)handle)->map(*(const QPoint*)p);
}

void QTransform_map2(QTransformH handle, PQtPointF retval, const QPointFH p)
{
	*(QPointF *)retval = ((QTransform *)handle)->map(*(const QPointF*)p);
}

void QTransform_map3(QTransformH handle, QLineH retval, const QLineH l)
{
	*(QLine *)retval = ((QTransform *)handle)->map(*(const QLine*)l);
}

void QTransform_map4(QTransformH handle, QLineFH retval, const QLineFH l)
{
	*(QLineF *)retval = ((QTransform *)handle)->map(*(const QLineF*)l);
}

void QTransform_map5(QTransformH handle, QPolygonFH retval, const QPolygonFH a)
{
	*(QPolygonF *)retval = ((QTransform *)handle)->map(*(const QPolygonF*)a);
}

void QTransform_map6(QTransformH handle, QPolygonH retval, const QPolygonH a)
{
	*(QPolygon *)retval = ((QTransform *)handle)->map(*(const QPolygon*)a);
}

void QTransform_map7(QTransformH handle, QRegionH retval, const QRegionH r)
{
	*(QRegion *)retval = ((QTransform *)handle)->map(*(const QRegion*)r);
}

void QTransform_map8(QTransformH handle, QPainterPathH retval, const QPainterPathH p)
{
	*(QPainterPath *)retval = ((QTransform *)handle)->map(*(const QPainterPath*)p);
}

void QTransform_mapToPolygon(QTransformH handle, QPolygonH retval, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	*(QPolygon *)retval = ((QTransform *)handle)->mapToPolygon(t_r);
}

void QTransform_mapRect(QTransformH handle, PRect retval, PRect AnonParam1)
{
	QRect t_retval;
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	t_retval = ((QTransform *)handle)->mapRect(t_AnonParam1);
	copyQRectToPRect(t_retval, retval);
}

void QTransform_mapRect2(QTransformH handle, QRectFH retval, const QRectFH AnonParam1)
{
	*(QRectF *)retval = ((QTransform *)handle)->mapRect(*(const QRectF*)AnonParam1);
}

void QTransform_map9(QTransformH handle, int x, int y, int* tx, int* ty)
{
	((QTransform *)handle)->map(x, y, tx, ty);
}

void QTransform_map10(QTransformH handle, qreal x, qreal y, qreal* tx, qreal* ty)
{
	((QTransform *)handle)->map(x, y, tx, ty);
}

void QTransform_fromTranslate(QTransformH retval, qreal dx, qreal dy)
{
	*(QTransform *)retval = QTransform::fromTranslate(dx, dy);
}

void QTransform_fromScale(QTransformH retval, qreal dx, qreal dy)
{
	*(QTransform *)retval = QTransform::fromScale(dx, dy);
}

