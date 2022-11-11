//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCURSOR_C_H
#define QCURSOR_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QCursorH QCursor_Create();
C_EXPORT void QCursor_Destroy(QCursorH handle);
C_EXPORT QCursorH QCursor_Create2(Qt::CursorShape shape);
C_EXPORT QCursorH QCursor_Create3(const QBitmapH bitmap, const QBitmapH mask, int hotX, int hotY);
C_EXPORT QCursorH QCursor_Create4(const QPixmapH pixmap, int hotX, int hotY);
C_EXPORT QCursorH QCursor_Create5(const QCursorH cursor);
C_EXPORT Qt::CursorShape QCursor_shape(QCursorH handle);
C_EXPORT void QCursor_setShape(QCursorH handle, Qt::CursorShape newShape);
C_EXPORT void QCursor_bitmap(QCursorH handle, QBitmapH retval);
C_EXPORT void QCursor_mask(QCursorH handle, QBitmapH retval);
C_EXPORT void QCursor_pixmap(QCursorH handle, QPixmapH retval);
C_EXPORT void QCursor_hotSpot(QCursorH handle, PQtPoint retval);
C_EXPORT void QCursor_pos(PQtPoint retval);
C_EXPORT void QCursor_pos2(PQtPoint retval, const QScreenH screen);
C_EXPORT void QCursor_setPos(int x, int y);
C_EXPORT void QCursor_setPos2(QScreenH screen, int x, int y);
C_EXPORT void QCursor_setPos3(const QPointH p);
C_EXPORT void QCursor_setPos4(QScreenH screen, const QPointH p);

#endif
