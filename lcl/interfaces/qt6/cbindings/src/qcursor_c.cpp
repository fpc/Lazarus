//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcursor_c.h"

QCursorH QCursor_Create()
{
	return (QCursorH) new QCursor();
}

void QCursor_Destroy(QCursorH handle)
{
	delete (QCursor *)handle;
}

QCursorH QCursor_Create2(Qt::CursorShape shape)
{
	return (QCursorH) new QCursor(shape);
}

QCursorH QCursor_Create3(const QBitmapH bitmap, const QBitmapH mask, int hotX, int hotY)
{
	return (QCursorH) new QCursor(*(const QBitmap*)bitmap, *(const QBitmap*)mask, hotX, hotY);
}

QCursorH QCursor_Create4(const QPixmapH pixmap, int hotX, int hotY)
{
	return (QCursorH) new QCursor(*(const QPixmap*)pixmap, hotX, hotY);
}

QCursorH QCursor_Create5(const QCursorH cursor)
{
	return (QCursorH) new QCursor(*(const QCursor*)cursor);
}

Qt::CursorShape QCursor_shape(QCursorH handle)
{
	return (Qt::CursorShape) ((QCursor *)handle)->shape();
}

void QCursor_setShape(QCursorH handle, Qt::CursorShape newShape)
{
	((QCursor *)handle)->setShape(newShape);
}

void QCursor_bitmap(QCursorH handle, QBitmapH retval)
{
  *(QBitmap *)retval = ((QCursor *)handle)->bitmap();
}

void QCursor_mask(QCursorH handle, QBitmapH retval)
{
  *(QBitmap *)retval = ((QCursor *)handle)->mask();
}

void QCursor_pixmap(QCursorH handle, QPixmapH retval)
{
	*(QPixmap *)retval = ((QCursor *)handle)->pixmap();
}

void QCursor_hotSpot(QCursorH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QCursor *)handle)->hotSpot();
}

void QCursor_pos(PQtPoint retval)
{
	*(QPoint *)retval = QCursor::pos();
}

void QCursor_pos2(PQtPoint retval, const QScreenH screen)
{
	*(QPoint *)retval = QCursor::pos((const QScreen*)screen);
}

void QCursor_setPos(int x, int y)
{
	QCursor::setPos(x, y);
}

void QCursor_setPos2(QScreenH screen, int x, int y)
{
	QCursor::setPos((QScreen*)screen, x, y);
}

void QCursor_setPos3(const QPointH p)
{
	QCursor::setPos(*(const QPoint*)p);
}

void QCursor_setPos4(QScreenH screen, const QPointH p)
{
	QCursor::setPos((QScreen*)screen, *(const QPoint*)p);
}

