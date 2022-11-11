//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDRAG_C_H
#define QDRAG_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QDragH QDrag_Create(QObjectH dragSource);
C_EXPORT void QDrag_Destroy(QDragH handle);
C_EXPORT void QDrag_setMimeData(QDragH handle, QMimeDataH data);
C_EXPORT QMimeDataH QDrag_mimeData(QDragH handle);
C_EXPORT void QDrag_setPixmap(QDragH handle, const QPixmapH AnonParam1);
C_EXPORT void QDrag_pixmap(QDragH handle, QPixmapH retval);
C_EXPORT void QDrag_setHotSpot(QDragH handle, const QPointH hotspot);
C_EXPORT void QDrag_hotSpot(QDragH handle, PQtPoint retval);
C_EXPORT QObjectH QDrag_source(QDragH handle);
C_EXPORT QObjectH QDrag_target(QDragH handle);
C_EXPORT Qt::DropAction QDrag_exec(QDragH handle, unsigned int supportedActions);
C_EXPORT Qt::DropAction QDrag_exec2(QDragH handle, unsigned int supportedActions, Qt::DropAction defaultAction);
C_EXPORT void QDrag_setDragCursor(QDragH handle, const QPixmapH cursor, Qt::DropAction action);
C_EXPORT void QDrag_dragCursor(QDragH handle, QPixmapH retval, Qt::DropAction action);
C_EXPORT unsigned int QDrag_supportedActions(QDragH handle);
C_EXPORT Qt::DropAction QDrag_defaultAction(QDragH handle);

#endif
