//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qgesture_c.h"

QGestureH QGesture_Create(QObjectH parent)
{
	return (QGestureH) new QGesture((QObject*)parent);
}

void QGesture_Destroy(QGestureH handle)
{
	delete (QGesture *)handle;
}

Qt::GestureType QGesture_gestureType(QGestureH handle)
{
	return (Qt::GestureType) ((QGesture *)handle)->gestureType();
}

Qt::GestureState QGesture_state(QGestureH handle)
{
	return (Qt::GestureState) ((QGesture *)handle)->state();
}

void QGesture_hotSpot(QGestureH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QGesture *)handle)->hotSpot();
}

void QGesture_setHotSpot(QGestureH handle, const QPointFH value)
{
	((QGesture *)handle)->setHotSpot(*(const QPointF*)value);
}

bool QGesture_hasHotSpot(QGestureH handle)
{
	return (bool) ((QGesture *)handle)->hasHotSpot();
}

void QGesture_unsetHotSpot(QGestureH handle)
{
	((QGesture *)handle)->unsetHotSpot();
}

void QGesture_setGestureCancelPolicy(QGestureH handle, QGesture::GestureCancelPolicy policy)
{
	((QGesture *)handle)->setGestureCancelPolicy(policy);
}

QGesture::GestureCancelPolicy QGesture_gestureCancelPolicy(QGestureH handle)
{
	return (QGesture::GestureCancelPolicy) ((QGesture *)handle)->gestureCancelPolicy();
}

QPanGestureH QPanGesture_Create(QObjectH parent)
{
	return (QPanGestureH) new QPanGesture((QObject*)parent);
}

void QPanGesture_Destroy(QPanGestureH handle)
{
	delete (QPanGesture *)handle;
}

void QPanGesture_lastOffset(QPanGestureH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QPanGesture *)handle)->lastOffset();
}

void QPanGesture_offset(QPanGestureH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QPanGesture *)handle)->offset();
}

void QPanGesture_delta(QPanGestureH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QPanGesture *)handle)->delta();
}

qreal QPanGesture_acceleration(QPanGestureH handle)
{
	return (qreal) ((QPanGesture *)handle)->acceleration();
}

void QPanGesture_setLastOffset(QPanGestureH handle, const QPointFH value)
{
	((QPanGesture *)handle)->setLastOffset(*(const QPointF*)value);
}

void QPanGesture_setOffset(QPanGestureH handle, const QPointFH value)
{
	((QPanGesture *)handle)->setOffset(*(const QPointF*)value);
}

void QPanGesture_setAcceleration(QPanGestureH handle, qreal value)
{
	((QPanGesture *)handle)->setAcceleration(value);
}

QPinchGestureH QPinchGesture_Create(QObjectH parent)
{
	return (QPinchGestureH) new QPinchGesture((QObject*)parent);
}

void QPinchGesture_Destroy(QPinchGestureH handle)
{
	delete (QPinchGesture *)handle;
}

unsigned int QPinchGesture_totalChangeFlags(QPinchGestureH handle)
{
	return (unsigned int) ((QPinchGesture *)handle)->totalChangeFlags();
}

void QPinchGesture_setTotalChangeFlags(QPinchGestureH handle, unsigned int value)
{
	((QPinchGesture *)handle)->setTotalChangeFlags((QPinchGesture::ChangeFlags)value);
}

unsigned int QPinchGesture_changeFlags(QPinchGestureH handle)
{
	return (unsigned int) ((QPinchGesture *)handle)->changeFlags();
}

void QPinchGesture_setChangeFlags(QPinchGestureH handle, unsigned int value)
{
	((QPinchGesture *)handle)->setChangeFlags((QPinchGesture::ChangeFlags)value);
}

void QPinchGesture_startCenterPoint(QPinchGestureH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QPinchGesture *)handle)->startCenterPoint();
}

void QPinchGesture_lastCenterPoint(QPinchGestureH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QPinchGesture *)handle)->lastCenterPoint();
}

void QPinchGesture_centerPoint(QPinchGestureH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QPinchGesture *)handle)->centerPoint();
}

void QPinchGesture_setStartCenterPoint(QPinchGestureH handle, const QPointFH value)
{
	((QPinchGesture *)handle)->setStartCenterPoint(*(const QPointF*)value);
}

void QPinchGesture_setLastCenterPoint(QPinchGestureH handle, const QPointFH value)
{
	((QPinchGesture *)handle)->setLastCenterPoint(*(const QPointF*)value);
}

void QPinchGesture_setCenterPoint(QPinchGestureH handle, const QPointFH value)
{
	((QPinchGesture *)handle)->setCenterPoint(*(const QPointF*)value);
}

qreal QPinchGesture_totalScaleFactor(QPinchGestureH handle)
{
	return (qreal) ((QPinchGesture *)handle)->totalScaleFactor();
}

qreal QPinchGesture_lastScaleFactor(QPinchGestureH handle)
{
	return (qreal) ((QPinchGesture *)handle)->lastScaleFactor();
}

qreal QPinchGesture_scaleFactor(QPinchGestureH handle)
{
	return (qreal) ((QPinchGesture *)handle)->scaleFactor();
}

void QPinchGesture_setTotalScaleFactor(QPinchGestureH handle, qreal value)
{
	((QPinchGesture *)handle)->setTotalScaleFactor(value);
}

void QPinchGesture_setLastScaleFactor(QPinchGestureH handle, qreal value)
{
	((QPinchGesture *)handle)->setLastScaleFactor(value);
}

void QPinchGesture_setScaleFactor(QPinchGestureH handle, qreal value)
{
	((QPinchGesture *)handle)->setScaleFactor(value);
}

qreal QPinchGesture_totalRotationAngle(QPinchGestureH handle)
{
	return (qreal) ((QPinchGesture *)handle)->totalRotationAngle();
}

qreal QPinchGesture_lastRotationAngle(QPinchGestureH handle)
{
	return (qreal) ((QPinchGesture *)handle)->lastRotationAngle();
}

qreal QPinchGesture_rotationAngle(QPinchGestureH handle)
{
	return (qreal) ((QPinchGesture *)handle)->rotationAngle();
}

void QPinchGesture_setTotalRotationAngle(QPinchGestureH handle, qreal value)
{
	((QPinchGesture *)handle)->setTotalRotationAngle(value);
}

void QPinchGesture_setLastRotationAngle(QPinchGestureH handle, qreal value)
{
	((QPinchGesture *)handle)->setLastRotationAngle(value);
}

void QPinchGesture_setRotationAngle(QPinchGestureH handle, qreal value)
{
	((QPinchGesture *)handle)->setRotationAngle(value);
}

QSwipeGestureH QSwipeGesture_Create(QObjectH parent)
{
	return (QSwipeGestureH) new QSwipeGesture((QObject*)parent);
}

void QSwipeGesture_Destroy(QSwipeGestureH handle)
{
	delete (QSwipeGesture *)handle;
}

QSwipeGesture::SwipeDirection QSwipeGesture_horizontalDirection(QSwipeGestureH handle)
{
	return (QSwipeGesture::SwipeDirection) ((QSwipeGesture *)handle)->horizontalDirection();
}

QSwipeGesture::SwipeDirection QSwipeGesture_verticalDirection(QSwipeGestureH handle)
{
	return (QSwipeGesture::SwipeDirection) ((QSwipeGesture *)handle)->verticalDirection();
}

qreal QSwipeGesture_swipeAngle(QSwipeGestureH handle)
{
	return (qreal) ((QSwipeGesture *)handle)->swipeAngle();
}

void QSwipeGesture_setSwipeAngle(QSwipeGestureH handle, qreal value)
{
	((QSwipeGesture *)handle)->setSwipeAngle(value);
}

QTapGestureH QTapGesture_Create(QObjectH parent)
{
	return (QTapGestureH) new QTapGesture((QObject*)parent);
}

void QTapGesture_Destroy(QTapGestureH handle)
{
	delete (QTapGesture *)handle;
}

void QTapGesture_position(QTapGestureH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QTapGesture *)handle)->position();
}

void QTapGesture_setPosition(QTapGestureH handle, const QPointFH pos)
{
	((QTapGesture *)handle)->setPosition(*(const QPointF*)pos);
}

QTapAndHoldGestureH QTapAndHoldGesture_Create(QObjectH parent)
{
	return (QTapAndHoldGestureH) new QTapAndHoldGesture((QObject*)parent);
}

void QTapAndHoldGesture_Destroy(QTapAndHoldGestureH handle)
{
	delete (QTapAndHoldGesture *)handle;
}

void QTapAndHoldGesture_position(QTapAndHoldGestureH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QTapAndHoldGesture *)handle)->position();
}

void QTapAndHoldGesture_setPosition(QTapAndHoldGestureH handle, const QPointFH pos)
{
	((QTapAndHoldGesture *)handle)->setPosition(*(const QPointF*)pos);
}

void QTapAndHoldGesture_setTimeout(int msecs)
{
	QTapAndHoldGesture::setTimeout(msecs);
}

int QTapAndHoldGesture_timeout()
{
	return (int) QTapAndHoldGesture::timeout();
}

QGestureEventH QGestureEvent_Create(PPtrIntArray gestures)
{
	QList<QGesture*> t_gestures;
	copyPtrIntArrayToQListTemplate(gestures, t_gestures);
	return (QGestureEventH) new QGestureEvent(t_gestures);
}

void QGestureEvent_Destroy(QGestureEventH handle)
{
	delete (QGestureEvent *)handle;
}

void QGestureEvent_gestures(QGestureEventH handle, PPtrIntArray retval)
{
	QList<QGesture*> t_retval;
	t_retval = ((QGestureEvent *)handle)->gestures();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QGestureH QGestureEvent_gesture(QGestureEventH handle, Qt::GestureType type)
{
	return (QGestureH) ((QGestureEvent *)handle)->gesture(type);
}

void QGestureEvent_activeGestures(QGestureEventH handle, PPtrIntArray retval)
{
	QList<QGesture*> t_retval;
	t_retval = ((QGestureEvent *)handle)->activeGestures();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGestureEvent_canceledGestures(QGestureEventH handle, PPtrIntArray retval)
{
	QList<QGesture*> t_retval;
	t_retval = ((QGestureEvent *)handle)->canceledGestures();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGestureEvent_setAccepted(QGestureEventH handle, QGestureH AnonParam1, bool AnonParam2)
{
	((QGestureEvent *)handle)->setAccepted((QGesture*)AnonParam1, AnonParam2);
}

void QGestureEvent_accept(QGestureEventH handle, QGestureH AnonParam1)
{
	((QGestureEvent *)handle)->accept((QGesture*)AnonParam1);
}

void QGestureEvent_ignore(QGestureEventH handle, QGestureH AnonParam1)
{
	((QGestureEvent *)handle)->ignore((QGesture*)AnonParam1);
}

bool QGestureEvent_isAccepted(QGestureEventH handle, QGestureH AnonParam1)
{
	return (bool) ((QGestureEvent *)handle)->isAccepted((QGesture*)AnonParam1);
}

void QGestureEvent_setAccepted2(QGestureEventH handle, Qt::GestureType AnonParam1, bool AnonParam2)
{
	((QGestureEvent *)handle)->setAccepted(AnonParam1, AnonParam2);
}

void QGestureEvent_accept2(QGestureEventH handle, Qt::GestureType AnonParam1)
{
	((QGestureEvent *)handle)->accept(AnonParam1);
}

void QGestureEvent_ignore2(QGestureEventH handle, Qt::GestureType AnonParam1)
{
	((QGestureEvent *)handle)->ignore(AnonParam1);
}

bool QGestureEvent_isAccepted2(QGestureEventH handle, Qt::GestureType AnonParam1)
{
	return (bool) ((QGestureEvent *)handle)->isAccepted(AnonParam1);
}

void QGestureEvent_setWidget(QGestureEventH handle, QWidgetH widget)
{
	((QGestureEvent *)handle)->setWidget((QWidget*)widget);
}

QWidgetH QGestureEvent_widget(QGestureEventH handle)
{
	return (QWidgetH) ((QGestureEvent *)handle)->widget();
}

void QGestureEvent_mapToGraphicsScene(QGestureEventH handle, PQtPointF retval, const QPointFH gesturePoint)
{
	*(QPointF *)retval = ((QGestureEvent *)handle)->mapToGraphicsScene(*(const QPointF*)gesturePoint);
}

