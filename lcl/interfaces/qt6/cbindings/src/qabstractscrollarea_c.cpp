//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractscrollarea_c.h"

QAbstractScrollAreaH QAbstractScrollArea_Create(QWidgetH parent)
{
	return (QAbstractScrollAreaH) new QAbstractScrollArea((QWidget*)parent);
}

void QAbstractScrollArea_Destroy(QAbstractScrollAreaH handle)
{
	delete (QAbstractScrollArea *)handle;
}

Qt::ScrollBarPolicy QAbstractScrollArea_verticalScrollBarPolicy(QAbstractScrollAreaH handle)
{
	return (Qt::ScrollBarPolicy) ((QAbstractScrollArea *)handle)->verticalScrollBarPolicy();
}

void QAbstractScrollArea_setVerticalScrollBarPolicy(QAbstractScrollAreaH handle, Qt::ScrollBarPolicy AnonParam1)
{
	((QAbstractScrollArea *)handle)->setVerticalScrollBarPolicy(AnonParam1);
}

QScrollBarH QAbstractScrollArea_verticalScrollBar(QAbstractScrollAreaH handle)
{
	return (QScrollBarH) ((QAbstractScrollArea *)handle)->verticalScrollBar();
}

void QAbstractScrollArea_setVerticalScrollBar(QAbstractScrollAreaH handle, QScrollBarH scrollbar)
{
	((QAbstractScrollArea *)handle)->setVerticalScrollBar((QScrollBar*)scrollbar);
}

Qt::ScrollBarPolicy QAbstractScrollArea_horizontalScrollBarPolicy(QAbstractScrollAreaH handle)
{
	return (Qt::ScrollBarPolicy) ((QAbstractScrollArea *)handle)->horizontalScrollBarPolicy();
}

void QAbstractScrollArea_setHorizontalScrollBarPolicy(QAbstractScrollAreaH handle, Qt::ScrollBarPolicy AnonParam1)
{
	((QAbstractScrollArea *)handle)->setHorizontalScrollBarPolicy(AnonParam1);
}

QScrollBarH QAbstractScrollArea_horizontalScrollBar(QAbstractScrollAreaH handle)
{
	return (QScrollBarH) ((QAbstractScrollArea *)handle)->horizontalScrollBar();
}

void QAbstractScrollArea_setHorizontalScrollBar(QAbstractScrollAreaH handle, QScrollBarH scrollbar)
{
	((QAbstractScrollArea *)handle)->setHorizontalScrollBar((QScrollBar*)scrollbar);
}

QWidgetH QAbstractScrollArea_cornerWidget(QAbstractScrollAreaH handle)
{
	return (QWidgetH) ((QAbstractScrollArea *)handle)->cornerWidget();
}

void QAbstractScrollArea_setCornerWidget(QAbstractScrollAreaH handle, QWidgetH widget)
{
	((QAbstractScrollArea *)handle)->setCornerWidget((QWidget*)widget);
}

void QAbstractScrollArea_addScrollBarWidget(QAbstractScrollAreaH handle, QWidgetH widget, unsigned int alignment)
{
	((QAbstractScrollArea *)handle)->addScrollBarWidget((QWidget*)widget, (Qt::Alignment)alignment);
}

void QAbstractScrollArea_scrollBarWidgets(QAbstractScrollAreaH handle, PPtrIntArray retval, unsigned int alignment)
{
	QList<QWidget*> t_retval;
	t_retval = ((QAbstractScrollArea *)handle)->scrollBarWidgets((Qt::Alignment)alignment);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QWidgetH QAbstractScrollArea_viewport(QAbstractScrollAreaH handle)
{
	return (QWidgetH) ((QAbstractScrollArea *)handle)->viewport();
}

void QAbstractScrollArea_setViewport(QAbstractScrollAreaH handle, QWidgetH widget)
{
	((QAbstractScrollArea *)handle)->setViewport((QWidget*)widget);
}

void QAbstractScrollArea_maximumViewportSize(QAbstractScrollAreaH handle, PSize retval)
{
	*(QSize *)retval = ((QAbstractScrollArea *)handle)->maximumViewportSize();
}

void QAbstractScrollArea_minimumSizeHint(QAbstractScrollAreaH handle, PSize retval)
{
	*(QSize *)retval = ((QAbstractScrollArea *)handle)->minimumSizeHint();
}

void QAbstractScrollArea_sizeHint(QAbstractScrollAreaH handle, PSize retval)
{
	*(QSize *)retval = ((QAbstractScrollArea *)handle)->sizeHint();
}

void QAbstractScrollArea_setupViewport(QAbstractScrollAreaH handle, QWidgetH viewport)
{
	((QAbstractScrollArea *)handle)->setupViewport((QWidget*)viewport);
}

