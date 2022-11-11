//******************************************************************************
//  Copyright (c) 2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qvectors_c.h"

QVector2DH QVector2D_Create()
{
  return (QVector2DH) new QVector2D();
}

QVector2DH QVector2D_Create2(QVector4DH vector)
{
  return (QVector2DH) new QVector2D(*(const QVector4D*)vector);
}

QVector2DH QVector2D_Create3(QVector3DH vector)
{
  return (QVector2DH) new QVector2D(*(const QVector3D*)vector);
}

QVector2DH QVector2D_Create4(PQtPointF point)
{
  return (QVector2DH) new QVector2D(*(const QPointF*)point);
}

QVector2DH QVector2D_Create5(PQtPoint point)
{
  return (QVector2DH) new QVector2D(*(const QPoint*)point);
}

QVector2DH QVector2D_Create6(float xpos, float ypos)
{
  return (QVector2DH) new QVector2D(xpos, ypos);
}

void QVector2D_Destroy(QVector2DH handle)
{
  delete (QVector2D *)handle;
}

float QVector2D_distanceToLine(QVector2DH handle, QVector2DH point, QVector2DH direction)
{
  return (float) ((QVector2D*)handle)->distanceToLine(*(const QVector2D*)point, *(const QVector2D*)direction);
}

float QVector2D_distanceToPoint(QVector2DH handle, QVector2DH point)
{
  return (float) ((QVector2D*)handle)->distanceToPoint(*(const QVector2D*)point);
}

bool QVector2D_isNull(QVector2DH handle)
{
  return (bool) ((QVector2D*)handle)->isNull();
}

float QVector2D_length(QVector2DH handle)
{
  return (float) ((QVector2D*)handle)->length();
}

float QVector2D_lengthSquared(QVector2DH handle)
{
  return (float) ((QVector2D*)handle)->lengthSquared();
}

void QVector2D_normalize(QVector2DH handle)
{
  ((QVector2D*)handle)->normalize();
}

void QVector2D_normalized(QVector2DH handle, QVector2DH retval)
{
  *(QVector2D*)retval = ((QVector2D*)handle)->normalized();
}

void QVector2D_setX(QVector2DH handle, float x)
{
  ((QVector2D*)handle)->setX(x);
}

void QVector2D_setY(QVector2DH handle, float y)
{
  ((QVector2D*)handle)->setY(y);
}

void QVector2D_toPoint(QVector2DH handle, PQtPoint retval)
{
  *(QPoint *)retval = ((QVector2D*)handle)->toPoint();
}

void QVector2D_toPointF(QVector2DH handle, PQtPointF retval)
{
  *(QPointF *)retval = ((QVector2D*)handle)->toPointF();
}

void QVector2D_toVector3D(QVector2DH handle, QVector3DH retval)
{
  *(QVector3D*)retval = ((QVector2D*)handle)->toVector3D();
}

void QVector2D_toVector4D(QVector2DH handle, QVector4DH retval)
{
  *(QVector4D*)retval = ((QVector2D*)handle)->toVector4D();
}

float QVector2D_x(QVector2DH handle)
{
  return (float) ((QVector2D*)handle)->x();
}

float QVector2D_y(QVector2DH handle)
{
  return (float) ((QVector2D*)handle)->y();
}

float QVector2D_dotProduct(QVector2DH v1, QVector2DH v2)
{
  return (float) QVector2D::dotProduct(*(QVector2D*)v1, *(QVector2D*)v2);
}


QVector3DH QVector3D_Create()
{
  return (QVector3DH) new QVector3D();
}

QVector3DH QVector3D_Create2(QVector4DH vector)
{
  return (QVector3DH) new QVector3D(*(const QVector4D*)vector);
}

QVector3DH QVector3D_Create3(QVector2DH vector)
{
  return (QVector3DH) new QVector3D(*(const QVector2D*)vector);
}

QVector3DH QVector3D_Create4(PQtPointF point)
{
  return (QVector3DH) new QVector3D(*(const QPointF*)point);
}

QVector3DH QVector3D_Create5(PQtPoint point)
{
  return (QVector3DH) new QVector3D(*(const QPoint*)point);
}

QVector3DH QVector3D_Create6(float xpos, float ypos, float zpos)
{
  return (QVector3DH) new QVector3D(xpos, ypos, zpos);
}

QVector3DH QVector3D_Create7(QVector2DH vector, float zpos)
{
  return (QVector3DH) new QVector3D(*(const QVector2D*)vector, zpos);
}

void QVector3D_Destroy(QVector3DH handle)
{
  delete (QVector3D *)handle;
}

float QVector3D_distanceToLine(QVector3DH handle, QVector3DH point, QVector3DH direction)
{
  return (float) ((QVector3D*)handle)->distanceToLine(*(const QVector3D*)point, *(const QVector3D*)direction);
}

float QVector3D_distanceToPlane(QVector3DH handle, QVector3DH plane, QVector3DH normal)
{
  return (float) ((QVector3D*)handle)->distanceToPlane(*(const QVector3D*)plane, *(const QVector3D*)normal);
}

float QVector3D_distanceToPlane2(QVector3DH handle, QVector3DH plane1, QVector3DH plane2, QVector3DH plane3)
{
  return (float) ((QVector3D*)handle)->distanceToPlane(*(const QVector3D*)plane1, *(const QVector3D*)plane2, *(const QVector3D*)plane3);
}

float QVector3D_distanceToPoint(QVector3DH handle, QVector3DH point)
{
  return (float) ((QVector3D*)handle)->distanceToPoint(*(const QVector3D*)point);
}

bool QVector3D_isNull(QVector3DH handle)
{
  return (bool) ((QVector3D*)handle)->isNull();
}

float QVector3D_length(QVector3DH handle)
{
  return (float) ((QVector3D*)handle)->length();
}

float QVector3D_lengthSquared(QVector3DH handle)
{
  return (float) ((QVector3D*)handle)->lengthSquared();
}

void QVector3D_normalize(QVector3DH handle)
{
  ((QVector3D*)handle)->normalize();
}

void QVector3D_normalized(QVector3DH handle, QVector3DH retval)
{
  *(QVector3D*)retval = ((QVector3D*)handle)->normalized();
}

void QVector3D_setX(QVector3DH handle, float x)
{
  ((QVector3D*)handle)->setX(x);
}

void QVector3D_setY(QVector3DH handle, float y)
{
  ((QVector3D*)handle)->setY(y);
}

void QVector3D_setZ(QVector3DH handle, float z)
{
  ((QVector3D*)handle)->setZ(z);
}

void QVector3D_toPoint(QVector3DH handle, PQtPoint retval)
{
  *(QPoint *)retval = ((QVector3D*)handle)->toPoint();
}

void QVector3D_toPointF(QVector3DH handle, PQtPointF retval)
{
  *(QPointF *)retval = ((QVector3D*)handle)->toPointF();
}

void QVector3D_toVector2D(QVector3DH handle, QVector2DH retval)
{
  *(QVector2D*)retval = ((QVector3D*)handle)->toVector2D();
}

void QVector3D_toVector4D(QVector3DH handle, QVector4DH retval)
{
  *(QVector4D*)retval = ((QVector3D*)handle)->toVector4D();
}

float QVector3D_x(QVector3DH handle)
{
  return (float) ((QVector3D*)handle)->x();
}

float QVector3D_y(QVector3DH handle)
{
  return (float) ((QVector3D*)handle)->y();
}

float QVector3D_z(QVector3DH handle)
{
  return (float) ((QVector3D*)handle)->z();
}

float QVector3D_dotProduct(QVector3DH v1, QVector3DH v2)
{
  return (float) QVector3D::dotProduct(*(QVector3D*)v1, *(QVector3D*)v2);
}

void QVector3D_crossProduct(QVector3DH v1, QVector3DH v2, QVector3DH retval)
{
  *(QVector3D*)retval = QVector3D::crossProduct(*(QVector3D*)v1, *(QVector3D*)v2);
}

void QVector3D_normal(QVector3DH v1, QVector3DH v2, QVector3DH retval)
{
  *(QVector3D*)retval = QVector3D::normal(*(QVector3D*)v1, *(QVector3D*)v2);
}

void QVector3D_normal2(QVector3DH v1, QVector3DH v2, QVector3DH v3, QVector3DH retval)
{
  *(QVector3D*)retval = QVector3D::normal(*(QVector3D*)v1, *(QVector3D*)v2, *(QVector3D*)v3);
}


QVector4DH QVector4D_Create()
{
  return (QVector4DH) new QVector4D();
}

QVector4DH QVector4D_Create2(QVector3DH vector)
{
  return (QVector4DH) new QVector4D(*(const QVector3D*) vector);
}

QVector4DH QVector4D_Create3(QVector2DH vector)
{
  return (QVector4DH) new QVector4D(*(const QVector2D*) vector);
}

QVector4DH QVector4D_Create4(PQtPointF point)
{
  return (QVector4DH) new QVector4D(*(const QPointF*) point);
}

QVector4DH QVector4D_Create5(PQtPoint point)
{
  return (QVector4DH) new QVector4D(*(const QPoint*) point);
}

QVector4DH QVector4D_Create6(float xpos, float ypos, float zpos, float wpos)
{
  return (QVector4DH) new QVector4D(xpos, ypos, zpos, wpos);
}

QVector4DH QVector4D_Create7(QVector3DH vector, float wpos)
{
  return (QVector4DH) new QVector4D(*(const QVector3D*)vector, wpos);
}

QVector4DH QVector4D_Create8(QVector2DH vector, float zpos, float wpos)
{
  return (QVector4DH) new QVector4D(*(const QVector2D*)vector, zpos, wpos);
}

void QVector4D_Destroy(QVector4DH handle)
{
  delete (QVector4D *)handle;
}

float QVector4D_length(QVector4DH handle)
{
  return (float) ((QVector3D*)handle)->length();
}

float QVector4D_lengthSquared(QVector4DH handle)
{
  return (float) ((QVector3D*)handle)->lengthSquared();
}

void QVector4D_normalize(QVector4DH handle)
{
  ((QVector4D*)handle)->normalize();
}

void QVector4D_normalized(QVector4DH handle, QVector4DH retval)
{
  *(QVector4D*)retval = ((QVector4D*)handle)->normalized();
}

void QVector4D_setW(QVector4DH handle, float w)
{
  ((QVector4D*)handle)->setW(w);
}

void QVector4D_setX(QVector4DH handle, float x)
{
  ((QVector4D*)handle)->setX(x);
}

void QVector4D_setY(QVector4DH handle, float y)
{
  ((QVector4D*)handle)->setY(y);
}

void QVector4D_setZ(QVector4DH handle, float z)
{
  ((QVector4D*)handle)->setZ(z);
}

float QVector4D_w(QVector4DH handle)
{
  return (float) ((QVector4D*)handle)->w();
}

float QVector4D_x(QVector4DH handle)
{
  return (float) ((QVector4D*)handle)->x();
}

float QVector4D_y(QVector4DH handle)
{
  return (float) ((QVector4D*)handle)->y();
}

float QVector4D_z(QVector4DH handle)
{
  return (float) ((QVector4D*)handle)->z();
}

void QVector4D_toPoint(QVector4DH handle, PQtPoint retval)
{
  *(QPoint *)retval = ((QVector4D*)handle)->toPoint();
}

void QVector4D_toPointF(QVector4DH handle, PQtPointF retval)
{
  *(QPointF *)retval = ((QVector4D*)handle)->toPointF();
}

void QVector4D_toVector2D(QVector4DH handle, QVector2DH retval)
{
  *(QVector2D*)retval = ((QVector4D*)handle)->toVector2D();
}

void QVector4D_toVector3D(QVector4DH handle, QVector3DH retval)
{
  *(QVector3D*)retval = ((QVector4D*)handle)->toVector3D();
}

void QVector4D_toVector2DAffine(QVector4DH handle, QVector2DH retval)
{
  *(QVector2D*)retval = ((QVector4D*)handle)->toVector2DAffine();
}

void QVector4D_toVector3DAffine(QVector4DH handle, QVector3DH retval)
{
  *(QVector3D*)retval = ((QVector4D*)handle)->toVector3DAffine();
}

float QVector4D_dotProduct(QVector4DH v1, QVector4DH v2)
{
  return (float) QVector4D::dotProduct(*(QVector4D*)v1, *(QVector4D*)v2);
}


QQuaternionH QQuaternion_Create()
{
  return (QQuaternionH) new QQuaternion();
}

QQuaternionH QQuaternion_Create2(QVector4DH vector)
{
  return (QQuaternionH) new QQuaternion(*(const QVector4D*) vector);
}

QQuaternionH QQuaternion_Create3(float scalar, QVector3DH vector)
{
  return (QQuaternionH) new QQuaternion(scalar, *(const QVector3D*) vector);
}

QQuaternionH QQuaternion_Create4(float scalar, float xpos, float ypos, float zpos)
{
  return (QQuaternionH) new QQuaternion(scalar, xpos, ypos, zpos);
}

void QQuaternion_Destroy(QQuaternionH handle)
{
  delete (QQuaternion *)handle;
}

void QQuaternion_conjugated(QQuaternionH handle, QQuaternionH retval)
{
  *(QQuaternion *)retval = ((QQuaternion*)handle)->conjugated();
}

void QQuaternion_getAxes(QQuaternionH handle, QVector3DH xAxis, QVector3DH yAxis, QVector3DH zAxis)
{
  ((QQuaternion*)handle)->getAxes((QVector3D *) xAxis, (QVector3D *) yAxis, (QVector3D *) zAxis);
}

void QQuaternion_getAxisAndAngle(QQuaternionH handle, float *x, float *y,float *z,float *angle)
{
  ((QQuaternion*)handle)->getAxisAndAngle(x, y, z, angle);
}

void QQuaternion_getAxisAndAngle2(QQuaternionH handle, QVector3DH axis,float *angle)
{
  ((QQuaternion*)handle)->getAxisAndAngle((QVector3D *)axis, angle);
}

void QQuaternion_getEulerAngles(QQuaternionH handle, float *pitch, float *yaw,float *roll)
{
  ((QQuaternion*)handle)->getEulerAngles(pitch, yaw, roll);
}

void QQuaternion_inverted(QQuaternionH handle, QQuaternionH retval)
{
  *(QQuaternion *)retval = ((QQuaternion*)handle)->inverted();
}

bool QQuaternion_isIdentity(QQuaternionH handle)
{
  return (bool) ((QQuaternion*)handle)->isIdentity();
}

bool QQuaternion_isNull(QQuaternionH handle)
{
  return (bool) ((QQuaternion*)handle)->isNull();
}

float QQuaternion_length(QQuaternionH handle)
{
  return (float) ((QQuaternion*)handle)->length();
}

float QQuaternion_lengthSquared(QQuaternionH handle)
{
  return (float) ((QQuaternion*)handle)->lengthSquared();
}

void QQuaternion_normalize(QQuaternionH handle)
{
  ((QQuaternion*)handle)->normalize();
}

void QQuaternion_normalized(QQuaternionH handle, QQuaternionH retval)
{
  *(QQuaternion *)retval = ((QQuaternion*)handle)->normalized();
}

void QQuaternion_rotatedVector(QQuaternionH handle, QVector3DH vector, QVector3DH retval)
{
  *(QVector3D *)retval = ((QQuaternion*)handle)->rotatedVector(*(const QVector3D*)vector);
}

float QQuaternion_scalar(QQuaternionH handle)
{
  return (float) ((QQuaternion*)handle)->scalar();
}

void QQuaternion_setScalar(QQuaternionH handle, float scalar)
{
  ((QQuaternion*)handle)->setScalar(scalar);
}

void QQuaternion_setVector(QQuaternionH handle, QVector3DH vector)
{
  ((QQuaternion*)handle)->setVector(*(const QVector3D*)vector);
}

void QQuaternion_setVector2(QQuaternionH handle, float x, float y, float z)
{
  ((QQuaternion*)handle)->setVector(x, y, z);
}

void QQuaternion_setX(QQuaternionH handle, float x)
{
  ((QQuaternion*)handle)->setX(x);
}

void QQuaternion_setY(QQuaternionH handle, float y)
{
  ((QQuaternion*)handle)->setY(y);
}

void QQuaternion_toEulerAngles(QQuaternionH handle, QVector3DH retval)
{
  *(QVector3D *)retval = ((QQuaternion*)handle)->toEulerAngles();
}

void QQuaternion_toVector4D(QQuaternionH handle, QVector4DH retval)
{
  *(QVector4D *)retval = ((QQuaternion*)handle)->toVector4D();
}

float QQuaternion_x(QQuaternionH handle)
{
  return (float) ((QQuaternion*)handle)->x();
}

float QQuaternion_y(QQuaternionH handle)
{
  return (float) ((QQuaternion*)handle)->y();
}

float QQuaternion_z(QQuaternionH handle)
{
  return (float) ((QQuaternion*)handle)->z();
}

