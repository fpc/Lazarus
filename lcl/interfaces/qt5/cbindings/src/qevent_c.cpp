//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qevent_c.h"

QInputEventH QInputEvent_Create(QEvent::Type type, unsigned int modifiers)
{
	return (QInputEventH) new QInputEvent(type, (Qt::KeyboardModifiers)modifiers);
}

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

const QPointFH QEnterEvent_localPos(QEnterEventH handle)
{
	return (const QPointFH) &((QEnterEvent *)handle)->localPos();
}

const QPointFH QEnterEvent_windowPos(QEnterEventH handle)
{
	return (const QPointFH) &((QEnterEvent *)handle)->windowPos();
}

const QPointFH QEnterEvent_screenPos(QEnterEventH handle)
{
	return (const QPointFH) &((QEnterEvent *)handle)->screenPos();
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

const QPointFH QMouseEvent_localPos(QMouseEventH handle)
{
	return (const QPointFH) &((QMouseEvent *)handle)->localPos();
}

const QPointFH QMouseEvent_windowPos(QMouseEventH handle)
{
	return (const QPointFH) &((QMouseEvent *)handle)->windowPos();
}

const QPointFH QMouseEvent_screenPos(QMouseEventH handle)
{
	return (const QPointFH) &((QMouseEvent *)handle)->screenPos();
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

const QPointFH QHoverEvent_posF(QHoverEventH handle)
{
	return (const QPointFH) &((QHoverEvent *)handle)->posF();
}

const QPointFH QHoverEvent_oldPosF(QHoverEventH handle)
{
	return (const QPointFH) &((QHoverEvent *)handle)->oldPosF();
}

QWheelEventH QWheelEvent_Create(const QPointFH pos, int delta, unsigned int buttons, unsigned int modifiers, Qt::Orientation orient)
{
	return (QWheelEventH) new QWheelEvent(*(const QPointF*)pos, delta, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers, orient);
}

void QWheelEvent_Destroy(QWheelEventH handle)
{
	delete (QWheelEvent *)handle;
}

QWheelEventH QWheelEvent_Create2(const QPointFH pos, const QPointFH globalPos, int delta, unsigned int buttons, unsigned int modifiers, Qt::Orientation orient)
{
	return (QWheelEventH) new QWheelEvent(*(const QPointF*)pos, *(const QPointF*)globalPos, delta, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers, orient);
}

QWheelEventH QWheelEvent_Create3(const QPointFH pos, const QPointFH globalPos, PQtPoint pixelDelta, PQtPoint angleDelta, int qt4Delta, Qt::Orientation qt4Orientation, unsigned int buttons, unsigned int modifiers)
{
	return (QWheelEventH) new QWheelEvent(*(const QPointF*)pos, *(const QPointF*)globalPos, *(QPoint *)pixelDelta, *(QPoint *)angleDelta, qt4Delta, qt4Orientation, (Qt::MouseButtons)buttons, (Qt::KeyboardModifiers)modifiers);
}

void QWheelEvent_pixelDelta(QWheelEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWheelEvent *)handle)->pixelDelta();
}

void QWheelEvent_angleDelta(QWheelEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWheelEvent *)handle)->angleDelta();
}

int QWheelEvent_delta(QWheelEventH handle)
{
	return (int) ((QWheelEvent *)handle)->delta();
}

Qt::Orientation QWheelEvent_orientation(QWheelEventH handle)
{
	return (Qt::Orientation) ((QWheelEvent *)handle)->orientation();
}

void QWheelEvent_pos(QWheelEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWheelEvent *)handle)->pos();
}

void QWheelEvent_globalPos(QWheelEventH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWheelEvent *)handle)->globalPos();
}

int QWheelEvent_x(QWheelEventH handle)
{
	return (int) ((QWheelEvent *)handle)->x();
}

int QWheelEvent_y(QWheelEventH handle)
{
	return (int) ((QWheelEvent *)handle)->y();
}

int QWheelEvent_globalX(QWheelEventH handle)
{
	return (int) ((QWheelEvent *)handle)->globalX();
}

int QWheelEvent_globalY(QWheelEventH handle)
{
	return (int) ((QWheelEvent *)handle)->globalY();
}

const QPointFH QWheelEvent_posF(QWheelEventH handle)
{
	return (const QPointFH) &((QWheelEvent *)handle)->posF();
}

const QPointFH QWheelEvent_globalPosF(QWheelEventH handle)
{
	return (const QPointFH) &((QWheelEvent *)handle)->globalPosF();
}

unsigned int QWheelEvent_buttons(QWheelEventH handle)
{
	return (unsigned int) ((QWheelEvent *)handle)->buttons();
}

QTabletEventH QTabletEvent_Create(QEvent::Type t, const QPointFH pos, const QPointFH globalPos, int device, int pointerType, qreal pressure, int xTilt, int yTilt, qreal tangentialPressure, qreal rotation, int z, unsigned int keyState, qint64 uniqueID)
{
	return (QTabletEventH) new QTabletEvent(t, *(const QPointF*)pos, *(const QPointF*)globalPos, device, pointerType, pressure, xTilt, yTilt, tangentialPressure, rotation, z, (Qt::KeyboardModifiers)keyState, uniqueID);
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

const QPointFH QTabletEvent_posF(QTabletEventH handle)
{
	return (const QPointFH) &((QTabletEvent *)handle)->posF();
}

const QPointFH QTabletEvent_globalPosF(QTabletEventH handle)
{
	return (const QPointFH) &((QTabletEvent *)handle)->globalPosF();
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

QTabletEvent::TabletDevice QTabletEvent_device(QTabletEventH handle)
{
	return (QTabletEvent::TabletDevice) ((QTabletEvent *)handle)->device();
}

QTabletEvent::PointerType QTabletEvent_pointerType(QTabletEventH handle)
{
	return (QTabletEvent::PointerType) ((QTabletEvent *)handle)->pointerType();
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

QInputMethodEventH QInputMethodEvent_Create3(const QInputMethodEventH other)
{
	return (QInputMethodEventH) new QInputMethodEvent(*(const QInputMethodEvent*)other);
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

const QPointFH QDropEvent_posF(QDropEventH handle)
{
	return (const QPointFH) &((QDropEvent *)handle)->posF();
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

QWindowH QTouchEvent_window(QTouchEventH handle)
{
	return (QWindowH) ((QTouchEvent *)handle)->window();
}

QObjectH QTouchEvent_target(QTouchEventH handle)
{
	return (QObjectH) ((QTouchEvent *)handle)->target();
}

unsigned int QTouchEvent_touchPointStates(QTouchEventH handle)
{
	return (unsigned int) ((QTouchEvent *)handle)->touchPointStates();
}

QTouchDeviceH QTouchEvent_device(QTouchEventH handle)
{
	return (QTouchDeviceH) ((QTouchEvent *)handle)->device();
}

void QTouchEvent_setWindow(QTouchEventH handle, QWindowH awindow)
{
	((QTouchEvent *)handle)->setWindow((QWindow*)awindow);
}

void QTouchEvent_setTarget(QTouchEventH handle, QObjectH atarget)
{
	((QTouchEvent *)handle)->setTarget((QObject*)atarget);
}

void QTouchEvent_setTouchPointStates(QTouchEventH handle, unsigned int aTouchPointStates)
{
	((QTouchEvent *)handle)->setTouchPointStates((Qt::TouchPointStates)aTouchPointStates);
}

void QTouchEvent_setDevice(QTouchEventH handle, QTouchDeviceH adevice)
{
	((QTouchEvent *)handle)->setDevice((QTouchDevice*)adevice);
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

