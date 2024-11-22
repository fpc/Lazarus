//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qevent_c.h"

void QInputEvent_Destroy(QInputEventH handle)
{
	delete (QInputEvent *)handle;
}

unsigned int QInputEvent_modifiers(QInputEventH handle)
{
	return (unsigned int) ((QInputEvent *)handle)->modifiers();
}

void QInputEvent_setModifiers(QInputEventH handle, unsigned int amodifiers)
{
	((QInputEvent *)handle)->setModifiers((Qt::KeyboardModifiers)amodifiers);
}

ulong QInputEvent_timestamp(QInputEventH handle)
{
	return (ulong) ((QInputEvent *)handle)->timestamp();
}

void QInputEvent_setTimestamp(QInputEventH handle, ulong atimestamp)
{
	((QInputEvent *)handle)->setTimestamp(atimestamp);
}

QInputDevice::DeviceType QInputEvent_deviceType(QInputEventH handle)
{
  return(QInputDevice::DeviceType) ((QInputEvent *)handle)->deviceType();
}

QPointingDeviceUniqueIdH QPointingDeviceUniqeId_Create()
{
  return (QPointingDeviceUniqueIdH) new QPointingDeviceUniqueId();
}

qint64 QPointingDeviceUniqeId_numericId(QPointingDeviceUniqueIdH handle)
{
  return (qint64) ((QPointingDeviceUniqueId *)handle)->numericId();
}

bool QPointingDeviceUniqeId_isValid(QPointingDeviceUniqueIdH handle)
{
  return (bool) ((QPointingDeviceUniqueId *)handle)->isValid();
}

void QPointingDeviceUniqeId_fromNumericId(qint64 id, QPointingDeviceUniqueIdH retval)
{
  *(QPointingDeviceUniqueId *)retval = QPointingDeviceUniqueId::fromNumericId(id);
}

QEventPointH QEventPoint_Create(const QEventPointH other)
{
  return (QEventPointH) new QEventPoint(*(const QEventPoint*) other);
}

QEventPointH QEventPoint_Create2(int pointid, QEventPoint::State state, const QPointFH scenePosition, const QPointFH globalPosition)
{
  return (QEventPointH) new QEventPoint(pointid, state, *(const QPointF*) scenePosition, *(const QPointF*) globalPosition);
}

void QEventPoint_Destroy(QEventPointH handle)
{
  delete (QEventPoint *)handle;
}

QPointingDeviceH QEventPoint_device(QEventPointH handle)
{
  return (QPointingDeviceH) ((QEventPoint *)handle)->device();
}

void QEventPoint_ellipseDiameters(QEventPointH handle, QSizeFH retval)
{
  *(QSizeF *)retval = ((QEventPoint *)handle)->ellipseDiameters();
}

void QEventPoint_globalGrabPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->globalGrabPosition();
}

void QEventPoint_globalLastPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->globalLastPosition();
}

void QEventPoint_globalPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->globalPosition();
}

void QEventPoint_globalPressPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->globalPressPosition();
}

void QEventPoint_grabPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->grabPosition();
}

int QEventPoint_id(QEventPointH handle)
{
  return (int) ((QEventPoint *)handle)->id();
}

bool QEventPoint_isAccepted(QEventPointH handle)
{
  return (bool) ((QEventPoint *)handle)->isAccepted();
}

void QEventPoint_lastPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->lastPosition();
}

ulong QEventPoint_lastTimestamp(QEventPointH handle)
{
  return (ulong) ((QEventPoint *)handle)->lastTimestamp();
}

void QEventPoint_normalizedPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->normalizedPosition();
}

void QEventPoint_position(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->position();
}

void QEventPoint_pressPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->pressPosition();
}

ulong QEventPoint_pressTimestamp(QEventPointH handle)
{
  return (ulong) ((QEventPoint *)handle)->pressTimestamp();
}

qreal QEventPoint_pressure(QEventPointH handle)
{
  return (qreal) ((QEventPoint *)handle)->pressure();
}

qreal QEventPoint_rotation(QEventPointH handle)
{
  return (qreal) ((QEventPoint *)handle)->rotation();
}

void QEventPoint_sceneGrabPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->sceneGrabPosition();
}

void QEventPoint_sceneLastPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->sceneLastPosition();
}

void QEventPoint_scenePosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->scenePosition();
}

void QEventPoint_scenePressPosition(QEventPointH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEventPoint *)handle)->scenePressPosition();
}

void QEventPoint_setAccepted(QEventPointH handle, bool accepted)
{
  ((QEventPoint *)handle)->setAccepted(accepted);
}

QEventPoint::State QEventPoint_state(QEventPointH handle)
{
  return (QEventPoint::State) ((QEventPoint *)handle)->state();
}

qreal QEventPoint_timeHeld(QEventPointH handle)
{
  return (qreal) ((QEventPoint *)handle)->timeHeld();
}

ulong QEventPoint_timestamp(QEventPointH handle)
{
  return (ulong) ((QEventPoint *)handle)->timestamp();
}

void QEventPoint_uniqueId(QEventPointH handle, QPointingDeviceUniqueIdH retval)
{
  *(QPointingDeviceUniqueId *)retval = ((QEventPoint *)handle)->uniqueId();
}

void QEventPoint_velocity(QEventPointH handle, QVector2DH retval)
{
  *(QVector2D *)retval = ((QEventPoint *)handle)->velocity();
}

bool QPointerEvent_allPointsAccepted(QPointerEventH handle)
{
  return (bool) ((QPointerEvent *)handle)->allPointsAccepted();
}

bool QPointerEvent_allPointsGrabbed(QPointerEventH handle)
{
  return (bool) ((QPointerEvent *)handle)->allPointsGrabbed();
}

void QPointerEvent_point(QPointerEventH handle, qsizetype i, QEventPointH retval)
{
  *(QEventPoint *)retval = ((QPointerEvent *)handle)->point(i);
}

QEventPointH QPointerEvent_pointById(QPointerEventH handle, int id)
{
  return (QEventPointH) ((QPointerEvent *)handle)->pointById(id);
}

qsizetype QPointerEvent_pointCount(QPointerEventH handle)
{
  return (qsizetype) ((QPointerEvent *)handle)->pointCount();
}

QPointingDevice::PointerType QPointerEvent_pointerType(QPointerEventH handle)
{
  return (QPointingDevice::PointerType) ((QPointerEvent *)handle)->pointerType();
}

QPointingDeviceH QPointerEvent_pointingDevice(QPointerEventH handle)
{
  return (QPointingDeviceH) ((QPointerEvent *)handle)->pointingDevice();
}

void QPointerEvent_points(QPointerEventH handle, PPtrIntArray retval)
{
	QList<QEventPoint> t_retval;
	t_retval = ((QPointerEvent *)handle)->points();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QPointerEvent_setAccepted(QPointerEventH handle, bool accepted)
{
  ((QPointerEvent *)handle)->setAccepted(accepted);
}

QInputDeviceH QInputDevice_Create()
{
  return (QInputDeviceH) new QInputDevice();
}

QInputDeviceH QInputDevice_Create2(QObjectH parent)
{
  return (QInputDeviceH) new QInputDevice((QObject*)parent);
}

QInputDeviceH QInputDevice_Create3(PWideString name, qint64 id, QInputDevice::DeviceType type, PWideString seatName, QObjectH parent)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	QString t_seatname;
	copyPWideStringToQString(seatName, t_seatname);

  return (QInputDeviceH) new QInputDevice(t_name, id, type, t_seatname, (QObject*)parent);
}

void QInputDevice_availableVirtualGeometry(QInputDeviceH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QInputDevice *)handle)->availableVirtualGeometry();
	copyQRectToPRect(t_retval, retval);
}

QInputDevice::Capabilities QInputDevice_capabilities(QInputDeviceH handle)
{
  return (QInputDevice::Capabilities) ((QInputDevice *)handle)->capabilities();
}

bool QInputDevice_hasCapability(QInputDeviceH handle, const QInputDevice::Capability capability)
{
  return (bool) ((QInputDevice *)handle)->hasCapability((QInputDevice::Capability)capability);
}

void QInputDevice_name(QInputDeviceH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QInputDevice *)handle)->name();
	copyQStringToPWideString(t_retval, retval);
}

void QInputDevice_seatName(QInputDeviceH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QInputDevice *)handle)->seatName();
	copyQStringToPWideString(t_retval, retval);
}

qint64 QInputDevice_systemId(QInputDeviceH handle)
{
  return (qint64) ((QInputDevice *)handle)->systemId();
}

QInputDevice::DeviceType QInputDevice_type(QInputDeviceH handle)
{
  return (QInputDevice::DeviceType) ((QInputDevice *)handle)->type();
}

void QInputDevice_devices(PPtrIntArray retval)
{
	QList<const QInputDevice*> t_retval;
	t_retval = QInputDevice::devices();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QInputDeviceH QInputDevice_primaryKeyboard(PWideString seatName)
{
	QString t_seatName;
	copyPWideStringToQString(seatName, t_seatName);
  return (QInputDeviceH) QInputDevice::primaryKeyboard(t_seatName);
}

QPointingDeviceH QPointingDevice_Create()
{
  return (QPointingDeviceH) new QPointingDevice();
}

QPointingDeviceH QPointingDevice_Create2(QObjectH parent)
{
  return (QPointingDeviceH) new QPointingDevice((QObject*)parent);
}

QPointingDeviceH QPointingDevice_Create3(PWideString name, qint64 id, QInputDevice::DeviceType type, QPointingDevice::PointerType pointerType, QInputDevice::Capabilities capabilities, int maxPoints, int buttonCount, PWideString seatName)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	QString t_seatName;
	copyPWideStringToQString(seatName, t_seatName);
  return (QPointingDeviceH) new QPointingDevice(t_name, id, type, pointerType, capabilities, maxPoints, buttonCount, t_seatName);
}

int QPointingDevice_buttonCount(QPointingDeviceH handle)
{
  return (int) ((QPointingDevice *)handle)->buttonCount();
}

int QPointingDevice_maximumPoints(QPointingDeviceH handle)
{
  return (int) ((QPointingDevice *)handle)->maximumPoints();
}

QPointingDevice::PointerType QPointingDevice_pointerType(QPointingDeviceH handle)
{
  return (QPointingDevice::PointerType) ((QPointingDevice *)handle)->pointerType();
}

void QPointingDevice_uniqueId(QPointingDeviceH handle, QPointingDeviceUniqueIdH retval)
{
  *(QPointingDeviceUniqueId*)retval = ((QPointingDevice *)handle)->uniqueId();
}

QPointingDeviceH QPointingDevice_primaryPointingDevice(const PWideString seatName)
{
	QString t_seatName;
	copyPWideStringToQString(seatName, t_seatName);
  return (QPointingDeviceH) QPointingDevice::primaryPointingDevice(t_seatName);
}


Qt::MouseButton QSinglePointEvent_button(QSinglePointEventH handle)
{
  return (Qt::MouseButton) ((QSinglePointEvent *)handle)->button();
}

Qt::MouseButtons QSinglePointEvent_buttons(QSinglePointEventH handle)
{
  return (Qt::MouseButtons) ((QSinglePointEvent *)handle)->buttons();
}

QObjectH QSinglePointEvent_exclusivePointGrabber(QSinglePointEventH handle)
{
  return (QObjectH) ((QSinglePointEvent *)handle)->exclusivePointGrabber();
}

void QSinglePointEvent_globalPosition(QSinglePointEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QSinglePointEvent *)handle)->globalPosition();
}

void QSinglePointEvent_position(QSinglePointEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QSinglePointEvent *)handle)->position();
}

void QSinglePointEvent_scenePosition(QSinglePointEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QSinglePointEvent *)handle)->scenePosition();
}

void QSinglePointEvent_setExclusivePointGrabber(QSinglePointEventH handle, QObjectH exclusiveGrabber)
{
  ((QSinglePointEvent *)handle)->setExclusivePointGrabber((QObject*)exclusiveGrabber);
}

bool QSinglePointEvent_isBeginEvent(QSinglePointEventH handle)
{
  return (bool) ((QSinglePointEvent *)handle)->isBeginEvent();
}

bool QSinglePointEvent_isEndEvent(QSinglePointEventH handle)
{
  return (bool) ((QSinglePointEvent *)handle)->isEndEvent();
}

bool QSinglePointEvent_isUpdateEvent(QSinglePointEventH handle)
{
  return (bool) ((QSinglePointEvent *)handle)->isUpdateEvent();
}

void QNativeGestureEvent_delta(QNativeGestureEventH handle, PQtPointF retval)
{
  *(QPointF *)retval = ((QNativeGestureEvent *)handle)->delta();
}

int QNativeGestureEvent_fingerCount(QNativeGestureEventH handle)
{
  return (int) ((QNativeGestureEvent *)handle)->fingerCount();
}

Qt::NativeGestureType QNativeGestureEvent_gestureType(QNativeGestureEventH handle)
{
  return (Qt::NativeGestureType) ((QNativeGestureEvent *)handle)->gestureType();
}

qreal QNativeGestureEvent_value(QNativeGestureEventH handle)
{
  return (qreal) ((QNativeGestureEvent *)handle)->value();
}

QEnterEventH QEnterEvent_Create(const QPointFH localPos, const QPointFH windowPos, const QPointFH screenPos)
{
	return (QEnterEventH) new QEnterEvent(*(const QPointF*)localPos, *(const QPointF*)windowPos, *(const QPointF*)screenPos);
}

void QEnterEvent_Destroy(QEnterEventH handle)
{
	delete (QEnterEvent *)handle;
}

void QEnterEvent_pos(QEnterEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QEnterEvent *)handle)->pos();
}

void QEnterEvent_globalPos(QEnterEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QEnterEvent *)handle)->globalPos();
}

int QEnterEvent_x(QEnterEventH handle)
{
	return (int) ((QEnterEvent *)handle)->x();
}

int QEnterEvent_y(QEnterEventH handle)
{
	return (int) ((QEnterEvent *)handle)->y();
}

int QEnterEvent_globalX(QEnterEventH handle)
{
	return (int) ((QEnterEvent *)handle)->globalX();
}

int QEnterEvent_globalY(QEnterEventH handle)
{
	return (int) ((QEnterEvent *)handle)->globalY();
}

void QEnterEvent_position(QEnterEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEnterEvent *)handle)->position();
}

void QEnterEvent_windowPos(QEnterEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEnterEvent *)handle)->windowPos();
}

void QEnterEvent_screenPos(QEnterEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QEnterEvent *)handle)->screenPos();
}

QMouseEventH QMouseEvent_Create(QEvent::Type type, const QPointFH localPos, Qt::MouseButton button, unsigned int buttons, unsigned int modifiers)
{
	return (QMouseEventH) new QMouseEvent(type, *(const QPointF*)localPos, button, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers);
}

void QMouseEvent_Destroy(QMouseEventH handle)
{
	delete (QMouseEvent *)handle;
}

QMouseEventH QMouseEvent_Create2(QEvent::Type type, const QPointFH localPos, const QPointFH screenPos, Qt::MouseButton button, unsigned int buttons, unsigned int modifiers)
{
	return (QMouseEventH) new QMouseEvent(type, *(const QPointF*)localPos, *(const QPointF*)screenPos, button, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers);
}

QMouseEventH QMouseEvent_Create3(QEvent::Type type, const QPointFH localPos, const QPointFH windowPos, const QPointFH screenPos, Qt::MouseButton button, unsigned int buttons, unsigned int modifiers)
{
	return (QMouseEventH) new QMouseEvent(type, *(const QPointF*)localPos, *(const QPointF*)windowPos, *(const QPointF*)screenPos, button, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers);
}

void QMouseEvent_pos(QMouseEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QMouseEvent *)handle)->pos();
}

void QMouseEvent_globalPos(QMouseEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QMouseEvent *)handle)->globalPos();
}

int QMouseEvent_x(QMouseEventH handle)
{
	return (int) ((QMouseEvent *)handle)->x();
}

int QMouseEvent_y(QMouseEventH handle)
{
	return (int) ((QMouseEvent *)handle)->y();
}

int QMouseEvent_globalX(QMouseEventH handle)
{
	return (int) ((QMouseEvent *)handle)->globalX();
}

int QMouseEvent_globalY(QMouseEventH handle)
{
	return (int) ((QMouseEvent *)handle)->globalY();
}

void QMouseEvent_position(QMouseEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QMouseEvent *)handle)->position();
}

void QMouseEvent_windowPos(QMouseEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QMouseEvent *)handle)->windowPos();
}

void QMouseEvent_screenPos(QMouseEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QMouseEvent *)handle)->screenPos();
}

Qt::MouseButton QMouseEvent_button(QMouseEventH handle)
{
	return (Qt::MouseButton) ((QMouseEvent *)handle)->button();
}

unsigned int QMouseEvent_buttons(QMouseEventH handle)
{
	return (unsigned int) ((QMouseEvent *)handle)->buttons();
}

QHoverEventH QHoverEvent_Create(QEvent::Type type, const QPointFH pos, const QPointFH oldPos, unsigned int modifiers)
{
	return (QHoverEventH) new QHoverEvent(type, *(const QPointF*)pos, *(const QPointF*)oldPos, (Qt::KeyboardModifiers)modifiers);
}

void QHoverEvent_Destroy(QHoverEventH handle)
{
	delete (QHoverEvent *)handle;
}

void QHoverEvent_pos(QHoverEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QHoverEvent *)handle)->pos();
}

void QHoverEvent_oldPos(QHoverEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QHoverEvent *)handle)->oldPos();
}

void QHoverEvent_position(QHoverEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QHoverEvent *)handle)->position();
}

void QHoverEvent_oldPosF(QHoverEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QHoverEvent *)handle)->oldPosF();
}

void QWheelEvent_Destroy(QWheelEventH handle)
{
	delete (QWheelEvent *)handle;
}

QWheelEventH QWheelEvent_Create(const QPointFH pos, const QPointFH globalPos, PQtPoint pixelDelta, PQtPoint angleDelta, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers, Qt::ScrollPhase phase, bool inverted, Qt::MouseEventSource source)
{
	return (QWheelEventH) new QWheelEvent(*(const QPointF*)pos, *(const QPointF*)globalPos, *(QPoint *)pixelDelta, *(QPoint *)angleDelta, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers, (Qt::ScrollPhase)phase, inverted, (Qt::MouseEventSource)source);
}

void QWheelEvent_pixelDelta(QWheelEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWheelEvent *)handle)->pixelDelta();
}

void QWheelEvent_angleDelta(QWheelEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWheelEvent *)handle)->angleDelta();
}

void QWheelEvent_pos(QWheelEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWheelEvent *)handle)->position().toPoint();
}

void QWheelEvent_globalPos(QWheelEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWheelEvent *)handle)->globalPosition().toPoint();
}

int QWheelEvent_x(QWheelEventH handle)
{
	return (int) ((QWheelEvent *)handle)->position().toPoint().x();
}

int QWheelEvent_y(QWheelEventH handle)
{
	return (int) ((QWheelEvent *)handle)->position().toPoint().y();
}

int QWheelEvent_globalX(QWheelEventH handle)
{
	return (int) ((QWheelEvent *)handle)->globalPosition().toPoint().x();
}

int QWheelEvent_globalY(QWheelEventH handle)
{
	return (int) ((QWheelEvent *)handle)->globalPosition().toPoint().y();
}

void QWheelEvent_position(QWheelEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QWheelEvent *)handle)->position();
}

void QWheelEvent_globalPosition(QWheelEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QWheelEvent *)handle)->globalPosition();
}

unsigned int QWheelEvent_buttons(QWheelEventH handle)
{
	return (unsigned int) ((QWheelEvent *)handle)->buttons();
}

bool QWheelEvent_inverted(QWheelEventH handle)
{
	return (bool) ((QWheelEvent *)handle)->inverted();
}

Qt::ScrollPhase QWheelEvent_phase(QWheelEventH handle)
{
  return (Qt::ScrollPhase) ((QWheelEvent *)handle)->phase();
}

Qt::MouseEventSource QWheelEvent_source(QWheelEventH handle)
{
  return (Qt::MouseEventSource) ((QWheelEvent *)handle)->source();
}

QTabletEventH QTabletEvent_Create(QEvent::Type t,  const QPointingDeviceH dev, const QPointFH pos, const QPointFH globalPos, qreal pressure, float xTilt, float yTilt, float tangentialPressure, qreal rotation, float z, unsigned int keyState, unsigned int button, unsigned int buttons)
{
	return (QTabletEventH) new QTabletEvent(t, (const QPointingDevice *) dev, *(const QPointF*)pos, *(const QPointF*)globalPos, pressure, xTilt, yTilt, tangentialPressure, rotation, z, (Qt::KeyboardModifiers)keyState, (Qt::MouseButton)button, (Qt::MouseButtons)buttons);
}

void QTabletEvent_Destroy(QTabletEventH handle)
{
	delete (QTabletEvent *)handle;
}

void QTabletEvent_pos(QTabletEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QTabletEvent *)handle)->pos();
}

void QTabletEvent_globalPos(QTabletEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QTabletEvent *)handle)->globalPos();
}

void QTabletEvent_position(QTabletEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QTabletEvent *)handle)->position();
}

void QTabletEvent_globalPosition(QTabletEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QTabletEvent *)handle)->globalPosition();
}

int QTabletEvent_x(QTabletEventH handle)
{
	return (int) ((QTabletEvent *)handle)->x();
}

int QTabletEvent_y(QTabletEventH handle)
{
	return (int) ((QTabletEvent *)handle)->y();
}

int QTabletEvent_globalX(QTabletEventH handle)
{
	return (int) ((QTabletEvent *)handle)->globalX();
}

int QTabletEvent_globalY(QTabletEventH handle)
{
	return (int) ((QTabletEvent *)handle)->globalY();
}

qreal QTabletEvent_hiResGlobalX(QTabletEventH handle)
{
	return (qreal) ((QTabletEvent *)handle)->hiResGlobalX();
}

qreal QTabletEvent_hiResGlobalY(QTabletEventH handle)
{
	return (qreal) ((QTabletEvent *)handle)->hiResGlobalY();
}

QInputDevice::DeviceType QTabletEvent_deviceType(QTabletEventH handle)
{
	return (QInputDevice::DeviceType) ((QTabletEvent *)handle)->deviceType();
}

QPointingDevice::PointerType QTabletEvent_pointerType(QTabletEventH handle)
{
	return (QPointingDevice::PointerType) ((QTabletEvent *)handle)->pointerType();
}

qint64 QTabletEvent_uniqueId(QTabletEventH handle)
{
	return (qint64) ((QTabletEvent *)handle)->uniqueId();
}

qreal QTabletEvent_pressure(QTabletEventH handle)
{
	return (qreal) ((QTabletEvent *)handle)->pressure();
}

int QTabletEvent_z(QTabletEventH handle)
{
	return (int) ((QTabletEvent *)handle)->z();
}

qreal QTabletEvent_tangentialPressure(QTabletEventH handle)
{
	return (qreal) ((QTabletEvent *)handle)->tangentialPressure();
}

qreal QTabletEvent_rotation(QTabletEventH handle)
{
	return (qreal) ((QTabletEvent *)handle)->rotation();
}

int QTabletEvent_xTilt(QTabletEventH handle)
{
	return (int) ((QTabletEvent *)handle)->xTilt();
}

int QTabletEvent_yTilt(QTabletEventH handle)
{
	return (int) ((QTabletEvent *)handle)->yTilt();
}

QKeyEventH QKeyEvent_Create(QEvent::Type type, int key, unsigned int modifiers, PWideString text, bool autorep, ushort count)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QKeyEventH) new QKeyEvent(type, key, (Qt::KeyboardModifiers)modifiers, t_text, autorep, count);
}

void QKeyEvent_Destroy(QKeyEventH handle)
{
	delete (QKeyEvent *)handle;
}

QKeyEventH QKeyEvent_Create2(QEvent::Type type, int key, unsigned int modifiers, quint32 nativeScanCode, quint32 nativeVirtualKey, quint32 nativeModifiers, PWideString text, bool autorep, ushort count)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QKeyEventH) new QKeyEvent(type, key, (Qt::KeyboardModifiers)modifiers, nativeScanCode, nativeVirtualKey, nativeModifiers, t_text, autorep, count);
}

int QKeyEvent_key(QKeyEventH handle)
{
	return (int) ((QKeyEvent *)handle)->key();
}

bool QKeyEvent_matches(QKeyEventH handle, QKeySequence::StandardKey key)
{
	return (bool) ((QKeyEvent *)handle)->matches(key);
}

unsigned int QKeyEvent_modifiers(QKeyEventH handle)
{
	return (unsigned int) ((QKeyEvent *)handle)->modifiers();
}

void QKeyEvent_text(QKeyEventH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QKeyEvent *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

bool QKeyEvent_isAutoRepeat(QKeyEventH handle)
{
	return (bool) ((QKeyEvent *)handle)->isAutoRepeat();
}

int QKeyEvent_count(QKeyEventH handle)
{
	return (int) ((QKeyEvent *)handle)->count();
}

quint32 QKeyEvent_nativeScanCode(QKeyEventH handle)
{
	return (quint32) ((QKeyEvent *)handle)->nativeScanCode();
}

quint32 QKeyEvent_nativeVirtualKey(QKeyEventH handle)
{
	return (quint32) ((QKeyEvent *)handle)->nativeVirtualKey();
}

quint32 QKeyEvent_nativeModifiers(QKeyEventH handle)
{
	return (quint32) ((QKeyEvent *)handle)->nativeModifiers();
}

void QKeyEvent_keyCombination(QKeyEventH handle, QKeyCombinationH retval)
{
 *(QKeyCombination *) retval = ((QKeyEvent *)handle)->keyCombination();
}

QFocusEventH QFocusEvent_Create(QEvent::Type type, Qt::FocusReason reason)
{
	return (QFocusEventH) new QFocusEvent(type, reason);
}

void QFocusEvent_Destroy(QFocusEventH handle)
{
	delete (QFocusEvent *)handle;
}

bool QFocusEvent_gotFocus(QFocusEventH handle)
{
	return (bool) ((QFocusEvent *)handle)->gotFocus();
}

bool QFocusEvent_lostFocus(QFocusEventH handle)
{
	return (bool) ((QFocusEvent *)handle)->lostFocus();
}

Qt::FocusReason QFocusEvent_reason(QFocusEventH handle)
{
	return (Qt::FocusReason) ((QFocusEvent *)handle)->reason();
}

QPaintEventH QPaintEvent_Create(const QRegionH paintRegion)
{
	return (QPaintEventH) new QPaintEvent(*(const QRegion*)paintRegion);
}

void QPaintEvent_Destroy(QPaintEventH handle)
{
	delete (QPaintEvent *)handle;
}

QPaintEventH QPaintEvent_Create2(PRect paintRect)
{
	QRect t_paintRect;
	copyPRectToQRect(paintRect, t_paintRect);
	return (QPaintEventH) new QPaintEvent(t_paintRect);
}

void QPaintEvent_rect(QPaintEventH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPaintEvent *)handle)->rect();
	copyQRectToPRect(t_retval, retval);
}

const QRegionH QPaintEvent_region(QPaintEventH handle)
{
	return (const QRegionH) &((QPaintEvent *)handle)->region();
}

QMoveEventH QMoveEvent_Create(const QPointH pos, const QPointH oldPos)
{
	return (QMoveEventH) new QMoveEvent(*(const QPoint*)pos, *(const QPoint*)oldPos);
}

void QMoveEvent_Destroy(QMoveEventH handle)
{
	delete (QMoveEvent *)handle;
}

const QPointH QMoveEvent_pos(QMoveEventH handle)
{
	return (const QPointH) &((QMoveEvent *)handle)->pos();
}

const QPointH QMoveEvent_oldPos(QMoveEventH handle)
{
	return (const QPointH) &((QMoveEvent *)handle)->oldPos();
}

QExposeEventH QExposeEvent_Create(const QRegionH rgn)
{
	return (QExposeEventH) new QExposeEvent(*(const QRegion*)rgn);
}

void QExposeEvent_Destroy(QExposeEventH handle)
{
	delete (QExposeEvent *)handle;
}

const QRegionH QExposeEvent_region(QExposeEventH handle)
{
	return (const QRegionH) &((QExposeEvent *)handle)->region();
}

QResizeEventH QResizeEvent_Create(const QSizeH size, const QSizeH oldSize)
{
	return (QResizeEventH) new QResizeEvent(*(const QSize*)size, *(const QSize*)oldSize);
}

void QResizeEvent_Destroy(QResizeEventH handle)
{
	delete (QResizeEvent *)handle;
}

const QSizeH QResizeEvent_size(QResizeEventH handle)
{
	return (const QSizeH) &((QResizeEvent *)handle)->size();
}

const QSizeH QResizeEvent_oldSize(QResizeEventH handle)
{
	return (const QSizeH) &((QResizeEvent *)handle)->oldSize();
}

QCloseEventH QCloseEvent_Create()
{
	return (QCloseEventH) new QCloseEvent();
}

void QCloseEvent_Destroy(QCloseEventH handle)
{
	delete (QCloseEvent *)handle;
}

QIconDragEventH QIconDragEvent_Create()
{
	return (QIconDragEventH) new QIconDragEvent();
}

void QIconDragEvent_Destroy(QIconDragEventH handle)
{
	delete (QIconDragEvent *)handle;
}

QShowEventH QShowEvent_Create()
{
	return (QShowEventH) new QShowEvent();
}

void QShowEvent_Destroy(QShowEventH handle)
{
	delete (QShowEvent *)handle;
}

QHideEventH QHideEvent_Create()
{
	return (QHideEventH) new QHideEvent();
}

void QHideEvent_Destroy(QHideEventH handle)
{
	delete (QHideEvent *)handle;
}

QContextMenuEventH QContextMenuEvent_Create(QContextMenuEvent::Reason reason, const QPointH pos, const QPointH globalPos, unsigned int modifiers)
{
	return (QContextMenuEventH) new QContextMenuEvent(reason, *(const QPoint*)pos, *(const QPoint*)globalPos, (Qt::KeyboardModifiers)modifiers);
}

void QContextMenuEvent_Destroy(QContextMenuEventH handle)
{
	delete (QContextMenuEvent *)handle;
}

QContextMenuEventH QContextMenuEvent_Create2(QContextMenuEvent::Reason reason, const QPointH pos, const QPointH globalPos)
{
	return (QContextMenuEventH) new QContextMenuEvent(reason, *(const QPoint*)pos, *(const QPoint*)globalPos);
}

QContextMenuEventH QContextMenuEvent_Create3(QContextMenuEvent::Reason reason, const QPointH pos)
{
	return (QContextMenuEventH) new QContextMenuEvent(reason, *(const QPoint*)pos);
}

int QContextMenuEvent_x(QContextMenuEventH handle)
{
	return (int) ((QContextMenuEvent *)handle)->x();
}

int QContextMenuEvent_y(QContextMenuEventH handle)
{
	return (int) ((QContextMenuEvent *)handle)->y();
}

int QContextMenuEvent_globalX(QContextMenuEventH handle)
{
	return (int) ((QContextMenuEvent *)handle)->globalX();
}

int QContextMenuEvent_globalY(QContextMenuEventH handle)
{
	return (int) ((QContextMenuEvent *)handle)->globalY();
}

const QPointH QContextMenuEvent_pos(QContextMenuEventH handle)
{
	return (const QPointH) &((QContextMenuEvent *)handle)->pos();
}

const QPointH QContextMenuEvent_globalPos(QContextMenuEventH handle)
{
	return (const QPointH) &((QContextMenuEvent *)handle)->globalPos();
}

QContextMenuEvent::Reason QContextMenuEvent_reason(QContextMenuEventH handle)
{
	return (QContextMenuEvent::Reason) ((QContextMenuEvent *)handle)->reason();
}

QInputMethodEventH QInputMethodEvent_Create()
{
	return (QInputMethodEventH) new QInputMethodEvent();
}

void QInputMethodEvent_Destroy(QInputMethodEventH handle)
{
	delete (QInputMethodEvent *)handle;
}

void QInputMethodEvent_setCommitString(QInputMethodEventH handle, PWideString commitString, int replaceFrom, int replaceLength)
{
	QString t_commitString;
	copyPWideStringToQString(commitString, t_commitString);
	((QInputMethodEvent *)handle)->setCommitString(t_commitString, replaceFrom, replaceLength);
}

void QInputMethodEvent_preeditString(QInputMethodEventH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QInputMethodEvent *)handle)->preeditString();
	copyQStringToPWideString(t_retval, retval);
}

void QInputMethodEvent_commitString(QInputMethodEventH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QInputMethodEvent *)handle)->commitString();
	copyQStringToPWideString(t_retval, retval);
}

int QInputMethodEvent_replacementStart(QInputMethodEventH handle)
{
	return (int) ((QInputMethodEvent *)handle)->replacementStart();
}

int QInputMethodEvent_replacementLength(QInputMethodEventH handle)
{
	return (int) ((QInputMethodEvent *)handle)->replacementLength();
}

QInputMethodQueryEventH QInputMethodQueryEvent_Create(unsigned int queries)
{
	return (QInputMethodQueryEventH) new QInputMethodQueryEvent((Qt::InputMethodQueries)queries);
}

void QInputMethodQueryEvent_Destroy(QInputMethodQueryEventH handle)
{
	delete (QInputMethodQueryEvent *)handle;
}

unsigned int QInputMethodQueryEvent_queries(QInputMethodQueryEventH handle)
{
	return (unsigned int) ((QInputMethodQueryEvent *)handle)->queries();
}

void QInputMethodQueryEvent_setValue(QInputMethodQueryEventH handle, Qt::InputMethodQuery query, const QVariantH value)
{
	((QInputMethodQueryEvent *)handle)->setValue(query, *(const QVariant*)value);
}

void QInputMethodQueryEvent_value(QInputMethodQueryEventH handle, QVariantH retval, Qt::InputMethodQuery query)
{
	*(QVariant *)retval = ((QInputMethodQueryEvent *)handle)->value(query);
}

QDropEventH QDropEvent_Create(const QPointFH pos, unsigned int actions, const QMimeDataH data, unsigned int buttons, unsigned int modifiers, QEvent::Type type)
{
	return (QDropEventH) new QDropEvent(*(const QPointF*)pos, (Qt::DropActions)actions, (const QMimeData*)data, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers, type);
}

void QDropEvent_Destroy(QDropEventH handle)
{
	delete (QDropEvent *)handle;
}

void QDropEvent_pos(QDropEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QDropEvent *)handle)->pos();
}

void QDropEvent_position(QDropEventH handle, QPointFH retval)
{
  *(QPointF *)retval = ((QDropEvent *)handle)->position();
}

unsigned int QDropEvent_mouseButtons(QDropEventH handle)
{
	return (unsigned int) ((QDropEvent *)handle)->mouseButtons();
}

unsigned int QDropEvent_keyboardModifiers(QDropEventH handle)
{
	return (unsigned int) ((QDropEvent *)handle)->keyboardModifiers();
}

unsigned int QDropEvent_possibleActions(QDropEventH handle)
{
	return (unsigned int) ((QDropEvent *)handle)->possibleActions();
}

Qt::DropAction QDropEvent_proposedAction(QDropEventH handle)
{
	return (Qt::DropAction) ((QDropEvent *)handle)->proposedAction();
}

void QDropEvent_acceptProposedAction(QDropEventH handle)
{
	((QDropEvent *)handle)->acceptProposedAction();
}

Qt::DropAction QDropEvent_dropAction(QDropEventH handle)
{
	return (Qt::DropAction) ((QDropEvent *)handle)->dropAction();
}

void QDropEvent_setDropAction(QDropEventH handle, Qt::DropAction action)
{
	((QDropEvent *)handle)->setDropAction(action);
}

QObjectH QDropEvent_source(QDropEventH handle)
{
	return (QObjectH) ((QDropEvent *)handle)->source();
}

const QMimeDataH QDropEvent_mimeData(QDropEventH handle)
{
	return (const QMimeDataH) ((QDropEvent *)handle)->mimeData();
}

QDragMoveEventH QDragMoveEvent_Create(const QPointH pos, unsigned int actions, const QMimeDataH data, unsigned int buttons, unsigned int modifiers, QEvent::Type type)
{
	return (QDragMoveEventH) new QDragMoveEvent(*(const QPoint*)pos, (Qt::DropActions)actions, (const QMimeData*)data, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers, type);
}

void QDragMoveEvent_Destroy(QDragMoveEventH handle)
{
	delete (QDragMoveEvent *)handle;
}

void QDragMoveEvent_answerRect(QDragMoveEventH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QDragMoveEvent *)handle)->answerRect();
	copyQRectToPRect(t_retval, retval);
}

void QDragMoveEvent_accept(QDragMoveEventH handle)
{
	((QDragMoveEvent *)handle)->accept();
}

void QDragMoveEvent_ignore(QDragMoveEventH handle)
{
	((QDragMoveEvent *)handle)->ignore();
}

void QDragMoveEvent_accept2(QDragMoveEventH handle, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QDragMoveEvent *)handle)->accept(t_r);
}

void QDragMoveEvent_ignore2(QDragMoveEventH handle, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QDragMoveEvent *)handle)->ignore(t_r);
}

QDragEnterEventH QDragEnterEvent_Create(const QPointH pos, unsigned int actions, const QMimeDataH data, unsigned int buttons, unsigned int modifiers)
{
	return (QDragEnterEventH) new QDragEnterEvent(*(const QPoint*)pos, (Qt::DropActions)actions, (const QMimeData*)data, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers);
}

void QDragEnterEvent_Destroy(QDragEnterEventH handle)
{
	delete (QDragEnterEvent *)handle;
}

QDragLeaveEventH QDragLeaveEvent_Create()
{
	return (QDragLeaveEventH) new QDragLeaveEvent();
}

void QDragLeaveEvent_Destroy(QDragLeaveEventH handle)
{
	delete (QDragLeaveEvent *)handle;
}

QHelpEventH QHelpEvent_Create(QEvent::Type type, const QPointH pos, const QPointH globalPos)
{
	return (QHelpEventH) new QHelpEvent(type, *(const QPoint*)pos, *(const QPoint*)globalPos);
}

void QHelpEvent_Destroy(QHelpEventH handle)
{
	delete (QHelpEvent *)handle;
}

int QHelpEvent_x(QHelpEventH handle)
{
	return (int) ((QHelpEvent *)handle)->x();
}

int QHelpEvent_y(QHelpEventH handle)
{
	return (int) ((QHelpEvent *)handle)->y();
}

int QHelpEvent_globalX(QHelpEventH handle)
{
	return (int) ((QHelpEvent *)handle)->globalX();
}

int QHelpEvent_globalY(QHelpEventH handle)
{
	return (int) ((QHelpEvent *)handle)->globalY();
}

const QPointH QHelpEvent_pos(QHelpEventH handle)
{
	return (const QPointH) &((QHelpEvent *)handle)->pos();
}

const QPointH QHelpEvent_globalPos(QHelpEventH handle)
{
	return (const QPointH) &((QHelpEvent *)handle)->globalPos();
}

QStatusTipEventH QStatusTipEvent_Create(PWideString tip)
{
	QString t_tip;
	copyPWideStringToQString(tip, t_tip);
	return (QStatusTipEventH) new QStatusTipEvent(t_tip);
}

void QStatusTipEvent_Destroy(QStatusTipEventH handle)
{
	delete (QStatusTipEvent *)handle;
}

void QStatusTipEvent_tip(QStatusTipEventH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStatusTipEvent *)handle)->tip();
	copyQStringToPWideString(t_retval, retval);
}

QWhatsThisClickedEventH QWhatsThisClickedEvent_Create(PWideString href)
{
	QString t_href;
	copyPWideStringToQString(href, t_href);
	return (QWhatsThisClickedEventH) new QWhatsThisClickedEvent(t_href);
}

void QWhatsThisClickedEvent_Destroy(QWhatsThisClickedEventH handle)
{
	delete (QWhatsThisClickedEvent *)handle;
}

void QWhatsThisClickedEvent_href(QWhatsThisClickedEventH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWhatsThisClickedEvent *)handle)->href();
	copyQStringToPWideString(t_retval, retval);
}

QActionEventH QActionEvent_Create(int type, QActionH action, QActionH before)
{
	return (QActionEventH) new QActionEvent(type, (QAction*)action, (QAction*)before);
}

void QActionEvent_Destroy(QActionEventH handle)
{
	delete (QActionEvent *)handle;
}

QActionH QActionEvent_action(QActionEventH handle)
{
	return (QActionH) ((QActionEvent *)handle)->action();
}

QActionH QActionEvent_before(QActionEventH handle)
{
	return (QActionH) ((QActionEvent *)handle)->before();
}

QFileOpenEventH QFileOpenEvent_Create(PWideString file)
{
	QString t_file;
	copyPWideStringToQString(file, t_file);
	return (QFileOpenEventH) new QFileOpenEvent(t_file);
}

void QFileOpenEvent_Destroy(QFileOpenEventH handle)
{
	delete (QFileOpenEvent *)handle;
}

QFileOpenEventH QFileOpenEvent_Create2(const QUrlH url)
{
	return (QFileOpenEventH) new QFileOpenEvent(*(const QUrl*)url);
}

void QFileOpenEvent_file(QFileOpenEventH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileOpenEvent *)handle)->file();
	copyQStringToPWideString(t_retval, retval);
}

void QFileOpenEvent_url(QFileOpenEventH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QFileOpenEvent *)handle)->url();
}

bool QFileOpenEvent_openFile(QFileOpenEventH handle, QFileH file, unsigned int flags)
{
	return (bool) ((QFileOpenEvent *)handle)->openFile(*(QFile*)file, (QIODevice::OpenMode)flags);
}

QShortcutEventH QShortcutEvent_Create(const QKeySequenceH key, int id, bool ambiguous)
{
	return (QShortcutEventH) new QShortcutEvent(*(const QKeySequence*)key, id, ambiguous);
}

void QShortcutEvent_Destroy(QShortcutEventH handle)
{
	delete (QShortcutEvent *)handle;
}

const QKeySequenceH QShortcutEvent_key(QShortcutEventH handle)
{
	return (const QKeySequenceH) &((QShortcutEvent *)handle)->key();
}

int QShortcutEvent_shortcutId(QShortcutEventH handle)
{
	return (int) ((QShortcutEvent *)handle)->shortcutId();
}

bool QShortcutEvent_isAmbiguous(QShortcutEventH handle)
{
	return (bool) ((QShortcutEvent *)handle)->isAmbiguous();
}

QWindowStateChangeEventH QWindowStateChangeEvent_Create(unsigned int aOldState, bool isOverride)
{
	return (QWindowStateChangeEventH) new QWindowStateChangeEvent((Qt::WindowStates)aOldState, isOverride);
}

void QWindowStateChangeEvent_Destroy(QWindowStateChangeEventH handle)
{
	delete (QWindowStateChangeEvent *)handle;
}

unsigned int QWindowStateChangeEvent_oldState(QWindowStateChangeEventH handle)
{
	return (unsigned int) ((QWindowStateChangeEvent *)handle)->oldState();
}

bool QWindowStateChangeEvent_isOverride(QWindowStateChangeEventH handle)
{
	return (bool) ((QWindowStateChangeEvent *)handle)->isOverride();
}

QObjectH QTouchEvent_target(QTouchEventH handle)
{
	return (QObjectH) ((QTouchEvent *)handle)->target();
}

QEventPoint::States QTouchEvent_touchPointStates(QTouchEventH handle)
{
	return (QEventPoint::States) ((QTouchEvent *)handle)->touchPointStates();
}

QInputDeviceH QTouchEvent_device(QTouchEventH handle)
{
	return (QInputDeviceH) ((QTouchEvent *)handle)->device();
}

bool QTouchEvent_isBeginEvent(QTouchEventH handle)
{
  return (bool) ((QTouchEvent *)handle)->isBeginEvent();
}

bool QTouchEvent_isEndEvent(QTouchEventH handle)
{
  return (bool) ((QTouchEvent *)handle)->isEndEvent();
}

bool QTouchEvent_isUpdateEvent(QTouchEventH handle)
{
  return (bool) ((QTouchEvent *)handle)->isUpdateEvent();
}


QScrollPrepareEventH QScrollPrepareEvent_Create(const QPointFH startPos)
{
	return (QScrollPrepareEventH) new QScrollPrepareEvent(*(const QPointF*)startPos);
}

void QScrollPrepareEvent_Destroy(QScrollPrepareEventH handle)
{
	delete (QScrollPrepareEvent *)handle;
}

void QScrollPrepareEvent_startPos(QScrollPrepareEventH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QScrollPrepareEvent *)handle)->startPos();
}

void QScrollPrepareEvent_viewportSize(QScrollPrepareEventH handle, QSizeFH retval)
{
	*(QSizeF *)retval = ((QScrollPrepareEvent *)handle)->viewportSize();
}

void QScrollPrepareEvent_contentPosRange(QScrollPrepareEventH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QScrollPrepareEvent *)handle)->contentPosRange();
}

void QScrollPrepareEvent_contentPos(QScrollPrepareEventH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QScrollPrepareEvent *)handle)->contentPos();
}

void QScrollPrepareEvent_setViewportSize(QScrollPrepareEventH handle, const QSizeFH size)
{
	((QScrollPrepareEvent *)handle)->setViewportSize(*(const QSizeF*)size);
}

void QScrollPrepareEvent_setContentPosRange(QScrollPrepareEventH handle, const QRectFH rect)
{
	((QScrollPrepareEvent *)handle)->setContentPosRange(*(const QRectF*)rect);
}

void QScrollPrepareEvent_setContentPos(QScrollPrepareEventH handle, const QPointFH pos)
{
	((QScrollPrepareEvent *)handle)->setContentPos(*(const QPointF*)pos);
}

QScrollEventH QScrollEvent_Create(const QPointFH contentPos, const QPointFH overshoot, QScrollEvent::ScrollState scrollState)
{
	return (QScrollEventH) new QScrollEvent(*(const QPointF*)contentPos, *(const QPointF*)overshoot, scrollState);
}

void QScrollEvent_Destroy(QScrollEventH handle)
{
	delete (QScrollEvent *)handle;
}

void QScrollEvent_contentPos(QScrollEventH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QScrollEvent *)handle)->contentPos();
}

void QScrollEvent_overshootDistance(QScrollEventH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QScrollEvent *)handle)->overshootDistance();
}

QScrollEvent::ScrollState QScrollEvent_scrollState(QScrollEventH handle)
{
	return (QScrollEvent::ScrollState) ((QScrollEvent *)handle)->scrollState();
}

QScreenOrientationChangeEventH QScreenOrientationChangeEvent_Create(QScreenH screen, Qt::ScreenOrientation orientation)
{
	return (QScreenOrientationChangeEventH) new QScreenOrientationChangeEvent((QScreen*)screen, orientation);
}

void QScreenOrientationChangeEvent_Destroy(QScreenOrientationChangeEventH handle)
{
	delete (QScreenOrientationChangeEvent *)handle;
}

QScreenH QScreenOrientationChangeEvent_screen(QScreenOrientationChangeEventH handle)
{
	return (QScreenH) ((QScreenOrientationChangeEvent *)handle)->screen();
}

Qt::ScreenOrientation QScreenOrientationChangeEvent_orientation(QScreenOrientationChangeEventH handle)
{
	return (Qt::ScreenOrientation) ((QScreenOrientationChangeEvent *)handle)->orientation();
}

QApplicationStateChangeEventH QApplicationStateChangeEvent_Create(Qt::ApplicationState state)
{
	return (QApplicationStateChangeEventH) new QApplicationStateChangeEvent(state);
}

void QApplicationStateChangeEvent_Destroy(QApplicationStateChangeEventH handle)
{
	delete (QApplicationStateChangeEvent *)handle;
}

Qt::ApplicationState QApplicationStateChangeEvent_applicationState(QApplicationStateChangeEventH handle)
{
	return (Qt::ApplicationState) ((QApplicationStateChangeEvent *)handle)->applicationState();
}

