//******************************************************************************
//  Copyright (c) 2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QVECTOR2D_C_H
#define QVECTOR2D_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QVector2DH QVector2D_Create();
C_EXPORT QVector2DH QVector2D_Create2(QVector4DH vector);
C_EXPORT QVector2DH QVector2D_Create3(QVector3DH vector);
C_EXPORT QVector2DH QVector2D_Create4(PQtPointF point);
C_EXPORT QVector2DH QVector2D_Create5(PQtPoint point);
C_EXPORT QVector2DH QVector2D_Create6(float xpos, float ypos);
C_EXPORT void QVector2D_Destroy(QVector2DH handle);
C_EXPORT float QVector2D_distanceToLine(QVector2DH handle, QVector2DH point, QVector2DH direction);
C_EXPORT float QVector2D_distanceToPoint(QVector2DH handle, QVector2DH point);
C_EXPORT bool QVector2D_isNull(QVector2DH handle);
C_EXPORT float QVector2D_length(QVector2DH handle);
C_EXPORT float QVector2D_lengthSquared(QVector2DH handle);
C_EXPORT void QVector2D_normalize(QVector2DH handle);
C_EXPORT void QVector2D_normalized(QVector2DH handle, QVector2DH retval);
C_EXPORT void QVector2D_setX(QVector2DH handle, float x);
C_EXPORT void QVector2D_setY(QVector2DH handle, float y);
C_EXPORT void QVector2D_toPoint(QVector2DH handle, PQtPoint retval);
C_EXPORT void QVector2D_toPointF(QVector2DH handle, PQtPointF retval);
C_EXPORT void QVector2D_toVector3D(QVector2DH handle, QVector3DH retval);
C_EXPORT void QVector2D_toVector4D(QVector2DH handle, QVector4DH retval);
C_EXPORT float QVector2D_x(QVector2DH handle);
C_EXPORT float QVector2D_y(QVector2DH handle);
C_EXPORT float QVector2D_dotProduct(QVector2DH v1, QVector2DH v2);

C_EXPORT QVector3DH QVector3D_Create();
C_EXPORT QVector3DH QVector3D_Create2(QVector4DH vector);
C_EXPORT QVector3DH QVector3D_Create3(QVector2DH vector);
C_EXPORT QVector3DH QVector3D_Create4(PQtPointF point);
C_EXPORT QVector3DH QVector3D_Create5(PQtPoint point);
C_EXPORT QVector3DH QVector3D_Create6(float xpos, float ypos, float zpos);
C_EXPORT QVector3DH QVector3D_Create7(QVector2DH vector, float zpos);
C_EXPORT void QVector3D_Destroy(QVector3DH handle);
C_EXPORT float QVector3D_distanceToLine(QVector3DH handle, QVector3DH point, QVector3DH direction);
C_EXPORT float QVector3D_distanceToPlane(QVector3DH handle, QVector3DH plane, QVector3DH normal);
C_EXPORT float QVector3D_distanceToPlane2(QVector3DH handle, QVector3DH plane1, QVector3DH plane2, QVector3DH plane3);
C_EXPORT float QVector3D_distanceToPoint(QVector3DH handle, QVector3DH point);
C_EXPORT bool QVector3D_isNull(QVector3DH handle);
C_EXPORT float QVector3D_length(QVector3DH handle);
C_EXPORT float QVector3D_lengthSquared(QVector3DH handle);
C_EXPORT void QVector3D_normalize(QVector3DH handle);
C_EXPORT void QVector3D_normalized(QVector3DH handle, QVector3DH retval);
C_EXPORT void QVector3D_setX(QVector3DH handle, float x);
C_EXPORT void QVector3D_setY(QVector3DH handle, float y);
C_EXPORT void QVector3D_setZ(QVector3DH handle, float z);
C_EXPORT void QVector3D_toPoint(QVector3DH handle, PQtPoint retval);
C_EXPORT void QVector3D_toPointF(QVector3DH handle, PQtPointF retval);
C_EXPORT void QVector3D_toVector2D(QVector3DH handle, QVector2DH retval);
C_EXPORT void QVector3D_toVector4D(QVector3DH handle, QVector4DH retval);
C_EXPORT float QVector3D_x(QVector3DH handle);
C_EXPORT float QVector3D_y(QVector3DH handle);
C_EXPORT float QVector3D_z(QVector3DH handle);
C_EXPORT float QVector3D_dotProduct(QVector3DH v1, QVector3DH v2);
C_EXPORT void QVector3D_crossProduct(QVector3DH v1, QVector3DH v2, QVector3DH retval);
C_EXPORT void QVector3D_normal(QVector3DH v1, QVector3DH v2, QVector3DH retval);
C_EXPORT void QVector3D_normal2(QVector3DH v1, QVector3DH v2, QVector3DH v3, QVector3DH retval);

C_EXPORT QVector4DH QVector4D_Create();
C_EXPORT QVector4DH QVector4D_Create2(QVector3DH vector);
C_EXPORT QVector4DH QVector4D_Create3(QVector2DH vector);
C_EXPORT QVector4DH QVector4D_Create4(PQtPointF point);
C_EXPORT QVector4DH QVector4D_Create5(PQtPoint point);
C_EXPORT QVector4DH QVector4D_Create6(float xpos, float ypos, float zpos, float wpos);
C_EXPORT QVector4DH QVector4D_Create7(QVector3DH vector, float wpos);
C_EXPORT QVector4DH QVector4D_Create8(QVector2DH vector, float zpos, float wpos);
C_EXPORT void QVector4D_Destroy(QVector4DH handle);
C_EXPORT float QVector4D_length(QVector4DH handle);
C_EXPORT float QVector4D_lengthSquared(QVector4DH handle);
C_EXPORT void QVector4D_normalize(QVector4DH handle);
C_EXPORT void QVector4D_normalized(QVector4DH handle, QVector4DH retval);
C_EXPORT void QVector4D_setW(QVector4DH handle, float w);
C_EXPORT void QVector4D_setX(QVector4DH handle, float x);
C_EXPORT void QVector4D_setY(QVector4DH handle, float y);
C_EXPORT void QVector4D_setZ(QVector4DH handle, float z);
C_EXPORT float QVector4D_w(QVector4DH handle);
C_EXPORT float QVector4D_x(QVector4DH handle);
C_EXPORT float QVector4D_y(QVector4DH handle);
C_EXPORT float QVector4D_z(QVector4DH handle);
C_EXPORT void QVector4D_toPoint(QVector4DH handle, PQtPoint retval);
C_EXPORT void QVector4D_toPointF(QVector4DH handle, PQtPointF retval);
C_EXPORT void QVector4D_toVector2D(QVector4DH handle, QVector2DH retval);
C_EXPORT void QVector4D_toVector3D(QVector4DH handle, QVector3DH retval);
C_EXPORT void QVector4D_toVector2DAffine(QVector4DH handle, QVector2DH retval);
C_EXPORT void QVector4D_toVector3DAffine(QVector4DH handle, QVector3DH retval);
C_EXPORT float QVector4D_dotProduct(QVector4DH v1, QVector4DH v2);

C_EXPORT QQuaternionH QQuaternion_Create();
C_EXPORT QQuaternionH QQuaternion_Create2(QVector4DH vector);
C_EXPORT QQuaternionH QQuaternion_Create3(float scalar, QVector3DH vector);
C_EXPORT QQuaternionH QQuaternion_Create4(float scalar, float xpos, float ypos, float zpos);
C_EXPORT void QQuaternion_Destroy(QQuaternionH handle);
C_EXPORT void QQuaternion_conjugated(QQuaternionH handle, QQuaternionH retval);
C_EXPORT void QQuaternion_getAxes(QQuaternionH handle, QVector3DH xAxis, QVector3DH yAxis, QVector3DH zAxis);
C_EXPORT void QQuaternion_getAxisAndAngle(QQuaternionH handle, float *x, float *y,float *z,float *angle);
C_EXPORT void QQuaternion_getAxisAndAngle2(QQuaternionH handle, QVector3DH axis,float *angle);
C_EXPORT void QQuaternion_getEulerAngles(QQuaternionH handle, float *pitch, float *yaw,float *roll);
C_EXPORT void QQuaternion_inverted(QQuaternionH handle, QQuaternionH retval);
C_EXPORT bool QQuaternion_isIdentity(QQuaternionH handle);
C_EXPORT bool QQuaternion_isNull(QQuaternionH handle);
C_EXPORT float QQuaternion_length(QQuaternionH handle);
C_EXPORT float QQuaternion_lengthSquared(QQuaternionH handle);
C_EXPORT void QQuaternion_normalize(QQuaternionH handle);
C_EXPORT void QQuaternion_normalized(QQuaternionH handle, QQuaternionH retval);
C_EXPORT void QQuaternion_rotatedVector(QQuaternionH handle, QVector3DH vector, QVector3DH retval);
C_EXPORT float QQuaternion_scalar(QQuaternionH handle);
C_EXPORT void QQuaternion_setScalar(QQuaternionH handle, float scalar);
C_EXPORT void QQuaternion_setVector(QQuaternionH handle, QVector3DH vector);
C_EXPORT void QQuaternion_setVector2(QQuaternionH handle, float x, float y, float z);
C_EXPORT void QQuaternion_setX(QQuaternionH handle, float x);
C_EXPORT void QQuaternion_setY(QQuaternionH handle, float y);
C_EXPORT void QQuaternion_toEulerAngles(QQuaternionH handle, QVector3DH retval);
C_EXPORT void QQuaternion_toVector4D(QQuaternionH handle, QVector4DH retval);
C_EXPORT float QQuaternion_x(QQuaternionH handle);
C_EXPORT float QQuaternion_y(QQuaternionH handle);
C_EXPORT float QQuaternion_z(QQuaternionH handle);


#endif
