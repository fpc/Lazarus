//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qdrag_c.h"

QDragH QDrag_Create(QObjectH dragSource)
{
	return (QDragH) new QDrag((QObject*)dragSource);
}

void QDrag_Destroy(QDragH handle)
{
	delete (QDrag *)handle;
}

void QDrag_setMimeData(QDragH handle, QMimeDataH data)
{
	((QDrag *)handle)->setMimeData((QMimeData*)data);
}

QMimeDataH QDrag_mimeData(QDragH handle)
{
	return (QMimeDataH) ((QDrag *)handle)->mimeData();
}

void QDrag_setPixmap(QDragH handle, const QPixmapH AnonParam1)
{
	((QDrag *)handle)->setPixmap(*(const QPixmap*)AnonParam1);
}

void QDrag_pixmap(QDragH handle, QPixmapH retval)
{
	*(QPixmap *)retval = ((QDrag *)handle)->pixmap();
}

void QDrag_setHotSpot(QDragH handle, const QPointH hotspot)
{
	((QDrag *)handle)->setHotSpot(*(const QPoint*)hotspot);
}

void QDrag_hotSpot(QDragH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QDrag *)handle)->hotSpot();
}

QObjectH QDrag_source(QDragH handle)
{
	return (QObjectH) ((QDrag *)handle)->source();
}

QObjectH QDrag_target(QDragH handle)
{
	return (QObjectH) ((QDrag *)handle)->target();
}

Qt::DropAction QDrag_exec(QDragH handle, unsigned int supportedActions)
{
	return (Qt::DropAction) ((QDrag *)handle)->exec((Qt::DropActions)supportedActions);
}

Qt::DropAction QDrag_exec2(QDragH handle, unsigned int supportedActions, Qt::DropAction defaultAction)
{
	return (Qt::DropAction) ((QDrag *)handle)->exec((Qt::DropActions)supportedActions, defaultAction);
}

void QDrag_setDragCursor(QDragH handle, const QPixmapH cursor, Qt::DropAction action)
{
	((QDrag *)handle)->setDragCursor(*(const QPixmap*)cursor, action);
}

void QDrag_dragCursor(QDragH handle, QPixmapH retval, Qt::DropAction action)
{
	*(QPixmap *)retval = ((QDrag *)handle)->dragCursor(action);
}

unsigned int QDrag_supportedActions(QDragH handle)
{
	return (unsigned int) ((QDrag *)handle)->supportedActions();
}

Qt::DropAction QDrag_defaultAction(QDragH handle)
{
	return (Qt::DropAction) ((QDrag *)handle)->defaultAction();
}

