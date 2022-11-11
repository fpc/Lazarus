//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGESTURE_C_H
#define QGESTURE_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QGestureH QGesture_Create(QObjectH parent);
C_EXPORT void QGesture_Destroy(QGestureH handle);
C_EXPORT Qt::GestureType QGesture_gestureType(QGestureH handle);
C_EXPORT Qt::GestureState QGesture_state(QGestureH handle);
C_EXPORT void QGesture_hotSpot(QGestureH handle, PQtPointF retval);
C_EXPORT void QGesture_setHotSpot(QGestureH handle, const QPointFH value);
C_EXPORT bool QGesture_hasHotSpot(QGestureH handle);
C_EXPORT void QGesture_unsetHotSpot(QGestureH handle);
C_EXPORT void QGesture_setGestureCancelPolicy(QGestureH handle, QGesture::GestureCancelPolicy policy);
C_EXPORT QGesture::GestureCancelPolicy QGesture_gestureCancelPolicy(QGestureH handle);
C_EXPORT QPanGestureH QPanGesture_Create(QObjectH parent);
C_EXPORT void QPanGesture_Destroy(QPanGestureH handle);
C_EXPORT void QPanGesture_lastOffset(QPanGestureH handle, PQtPointF retval);
C_EXPORT void QPanGesture_offset(QPanGestureH handle, PQtPointF retval);
C_EXPORT void QPanGesture_delta(QPanGestureH handle, PQtPointF retval);
C_EXPORT qreal QPanGesture_acceleration(QPanGestureH handle);
C_EXPORT void QPanGesture_setLastOffset(QPanGestureH handle, const QPointFH value);
C_EXPORT void QPanGesture_setOffset(QPanGestureH handle, const QPointFH value);
C_EXPORT void QPanGesture_setAcceleration(QPanGestureH handle, qreal value);
C_EXPORT QPinchGestureH QPinchGesture_Create(QObjectH parent);
C_EXPORT void QPinchGesture_Destroy(QPinchGestureH handle);
C_EXPORT unsigned int QPinchGesture_totalChangeFlags(QPinchGestureH handle);
C_EXPORT void QPinchGesture_setTotalChangeFlags(QPinchGestureH handle, unsigned int value);
C_EXPORT unsigned int QPinchGesture_changeFlags(QPinchGestureH handle);
C_EXPORT void QPinchGesture_setChangeFlags(QPinchGestureH handle, unsigned int value);
C_EXPORT void QPinchGesture_startCenterPoint(QPinchGestureH handle, PQtPointF retval);
C_EXPORT void QPinchGesture_lastCenterPoint(QPinchGestureH handle, PQtPointF retval);
C_EXPORT void QPinchGesture_centerPoint(QPinchGestureH handle, PQtPointF retval);
C_EXPORT void QPinchGesture_setStartCenterPoint(QPinchGestureH handle, const QPointFH value);
C_EXPORT void QPinchGesture_setLastCenterPoint(QPinchGestureH handle, const QPointFH value);
C_EXPORT void QPinchGesture_setCenterPoint(QPinchGestureH handle, const QPointFH value);
C_EXPORT qreal QPinchGesture_totalScaleFactor(QPinchGestureH handle);
C_EXPORT qreal QPinchGesture_lastScaleFactor(QPinchGestureH handle);
C_EXPORT qreal QPinchGesture_scaleFactor(QPinchGestureH handle);
C_EXPORT void QPinchGesture_setTotalScaleFactor(QPinchGestureH handle, qreal value);
C_EXPORT void QPinchGesture_setLastScaleFactor(QPinchGestureH handle, qreal value);
C_EXPORT void QPinchGesture_setScaleFactor(QPinchGestureH handle, qreal value);
C_EXPORT qreal QPinchGesture_totalRotationAngle(QPinchGestureH handle);
C_EXPORT qreal QPinchGesture_lastRotationAngle(QPinchGestureH handle);
C_EXPORT qreal QPinchGesture_rotationAngle(QPinchGestureH handle);
C_EXPORT void QPinchGesture_setTotalRotationAngle(QPinchGestureH handle, qreal value);
C_EXPORT void QPinchGesture_setLastRotationAngle(QPinchGestureH handle, qreal value);
C_EXPORT void QPinchGesture_setRotationAngle(QPinchGestureH handle, qreal value);
C_EXPORT QSwipeGestureH QSwipeGesture_Create(QObjectH parent);
C_EXPORT void QSwipeGesture_Destroy(QSwipeGestureH handle);
C_EXPORT QSwipeGesture::SwipeDirection QSwipeGesture_horizontalDirection(QSwipeGestureH handle);
C_EXPORT QSwipeGesture::SwipeDirection QSwipeGesture_verticalDirection(QSwipeGestureH handle);
C_EXPORT qreal QSwipeGesture_swipeAngle(QSwipeGestureH handle);
C_EXPORT void QSwipeGesture_setSwipeAngle(QSwipeGestureH handle, qreal value);
C_EXPORT QTapGestureH QTapGesture_Create(QObjectH parent);
C_EXPORT void QTapGesture_Destroy(QTapGestureH handle);
C_EXPORT void QTapGesture_position(QTapGestureH handle, PQtPointF retval);
C_EXPORT void QTapGesture_setPosition(QTapGestureH handle, const QPointFH pos);
C_EXPORT QTapAndHoldGestureH QTapAndHoldGesture_Create(QObjectH parent);
C_EXPORT void QTapAndHoldGesture_Destroy(QTapAndHoldGestureH handle);
C_EXPORT void QTapAndHoldGesture_position(QTapAndHoldGestureH handle, PQtPointF retval);
C_EXPORT void QTapAndHoldGesture_setPosition(QTapAndHoldGestureH handle, const QPointFH pos);
C_EXPORT void QTapAndHoldGesture_setTimeout(int msecs);
C_EXPORT int QTapAndHoldGesture_timeout();
C_EXPORT QGestureEventH QGestureEvent_Create(PPtrIntArray gestures);
C_EXPORT void QGestureEvent_Destroy(QGestureEventH handle);
C_EXPORT void QGestureEvent_gestures(QGestureEventH handle, PPtrIntArray retval);
C_EXPORT QGestureH QGestureEvent_gesture(QGestureEventH handle, Qt::GestureType type);
C_EXPORT void QGestureEvent_activeGestures(QGestureEventH handle, PPtrIntArray retval);
C_EXPORT void QGestureEvent_canceledGestures(QGestureEventH handle, PPtrIntArray retval);
C_EXPORT void QGestureEvent_setAccepted(QGestureEventH handle, QGestureH AnonParam1, bool AnonParam2);
C_EXPORT void QGestureEvent_accept(QGestureEventH handle, QGestureH AnonParam1);
C_EXPORT void QGestureEvent_ignore(QGestureEventH handle, QGestureH AnonParam1);
C_EXPORT bool QGestureEvent_isAccepted(QGestureEventH handle, QGestureH AnonParam1);
C_EXPORT void QGestureEvent_setAccepted2(QGestureEventH handle, Qt::GestureType AnonParam1, bool AnonParam2);
C_EXPORT void QGestureEvent_accept2(QGestureEventH handle, Qt::GestureType AnonParam1);
C_EXPORT void QGestureEvent_ignore2(QGestureEventH handle, Qt::GestureType AnonParam1);
C_EXPORT bool QGestureEvent_isAccepted2(QGestureEventH handle, Qt::GestureType AnonParam1);
C_EXPORT void QGestureEvent_setWidget(QGestureEventH handle, QWidgetH widget);
C_EXPORT QWidgetH QGestureEvent_widget(QGestureEventH handle);
C_EXPORT void QGestureEvent_mapToGraphicsScene(QGestureEventH handle, PQtPointF retval, const QPointFH gesturePoint);

#endif
