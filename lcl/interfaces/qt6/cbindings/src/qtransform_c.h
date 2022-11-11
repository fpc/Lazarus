//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTRANSFORM_C_H
#define QTRANSFORM_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QTransformH QTransform_Create(Qt::Initialization AnonParam1);
C_EXPORT void QTransform_Destroy(QTransformH handle);
C_EXPORT QTransformH QTransform_Create2();
C_EXPORT QTransformH QTransform_Create3(qreal h11, qreal h12, qreal h13, qreal h21, qreal h22, qreal h23, qreal h31, qreal h32, qreal h33);
C_EXPORT QTransformH QTransform_Create4(qreal h11, qreal h12, qreal h21, qreal h22, qreal dx, qreal dy);
C_EXPORT bool QTransform_isAffine(QTransformH handle);
C_EXPORT bool QTransform_isIdentity(QTransformH handle);
C_EXPORT bool QTransform_isInvertible(QTransformH handle);
C_EXPORT bool QTransform_isScaling(QTransformH handle);
C_EXPORT bool QTransform_isRotating(QTransformH handle);
C_EXPORT bool QTransform_isTranslating(QTransformH handle);
C_EXPORT QTransform::TransformationType QTransform_type(QTransformH handle);
C_EXPORT qreal QTransform_determinant(QTransformH handle);
C_EXPORT qreal QTransform_m11(QTransformH handle);
C_EXPORT qreal QTransform_m12(QTransformH handle);
C_EXPORT qreal QTransform_m13(QTransformH handle);
C_EXPORT qreal QTransform_m21(QTransformH handle);
C_EXPORT qreal QTransform_m22(QTransformH handle);
C_EXPORT qreal QTransform_m23(QTransformH handle);
C_EXPORT qreal QTransform_m31(QTransformH handle);
C_EXPORT qreal QTransform_m32(QTransformH handle);
C_EXPORT qreal QTransform_m33(QTransformH handle);
C_EXPORT qreal QTransform_dx(QTransformH handle);
C_EXPORT qreal QTransform_dy(QTransformH handle);
C_EXPORT void QTransform_setMatrix(QTransformH handle, qreal m11, qreal m12, qreal m13, qreal m21, qreal m22, qreal m23, qreal m31, qreal m32, qreal m33);
C_EXPORT void QTransform_inverted(QTransformH handle, QTransformH retval, bool* invertible);
C_EXPORT void QTransform_adjoint(QTransformH handle, QTransformH retval);
C_EXPORT void QTransform_transposed(QTransformH handle, QTransformH retval);
C_EXPORT QTransformH QTransform_translate(QTransformH handle, qreal dx, qreal dy);
C_EXPORT QTransformH QTransform_scale(QTransformH handle, qreal sx, qreal sy);
C_EXPORT QTransformH QTransform_shear(QTransformH handle, qreal sh, qreal sv);
C_EXPORT QTransformH QTransform_rotate(QTransformH handle, qreal a, Qt::Axis axis);
C_EXPORT QTransformH QTransform_rotateRadians(QTransformH handle, qreal a, Qt::Axis axis);
C_EXPORT bool QTransform_squareToQuad(const QPolygonFH square, QTransformH result);
C_EXPORT bool QTransform_quadToSquare(const QPolygonFH quad, QTransformH result);
C_EXPORT bool QTransform_quadToQuad(const QPolygonFH one, const QPolygonFH two, QTransformH result);
C_EXPORT void QTransform_reset(QTransformH handle);
C_EXPORT void QTransform_map(QTransformH handle, PQtPoint retval, const QPointH p);
C_EXPORT void QTransform_map2(QTransformH handle, PQtPointF retval, const QPointFH p);
C_EXPORT void QTransform_map3(QTransformH handle, QLineH retval, const QLineH l);
C_EXPORT void QTransform_map4(QTransformH handle, QLineFH retval, const QLineFH l);
C_EXPORT void QTransform_map5(QTransformH handle, QPolygonFH retval, const QPolygonFH a);
C_EXPORT void QTransform_map6(QTransformH handle, QPolygonH retval, const QPolygonH a);
C_EXPORT void QTransform_map7(QTransformH handle, QRegionH retval, const QRegionH r);
C_EXPORT void QTransform_map8(QTransformH handle, QPainterPathH retval, const QPainterPathH p);
C_EXPORT void QTransform_mapToPolygon(QTransformH handle, QPolygonH retval, PRect r);
C_EXPORT void QTransform_mapRect(QTransformH handle, PRect retval, PRect AnonParam1);
C_EXPORT void QTransform_mapRect2(QTransformH handle, QRectFH retval, const QRectFH AnonParam1);
C_EXPORT void QTransform_map9(QTransformH handle, int x, int y, int* tx, int* ty);
C_EXPORT void QTransform_map10(QTransformH handle, qreal x, qreal y, qreal* tx, qreal* ty);
C_EXPORT void QTransform_fromTranslate(QTransformH retval, qreal dx, qreal dy);
C_EXPORT void QTransform_fromScale(QTransformH retval, qreal dx, qreal dy);

#endif
