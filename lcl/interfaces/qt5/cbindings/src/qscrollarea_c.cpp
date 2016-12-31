//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qscrollarea_c.h"

QScrollAreaH QScrollArea_Create(QWidgetH parent)
{
	return (QScrollAreaH) new QScrollArea((QWidget*)parent);
}

void QScrollArea_Destroy(QScrollAreaH handle)
{
	delete (QScrollArea *)handle;
}

QWidgetH QScrollArea_widget(QScrollAreaH handle)
{
	return (QWidgetH) ((QScrollArea *)handle)->widget();
}

void QScrollArea_setWidget(QScrollAreaH handle, QWidgetH widget)
{
	((QScrollArea *)handle)->setWidget((QWidget*)widget);
}

QWidgetH QScrollArea_takeWidget(QScrollAreaH handle)
{
	return (QWidgetH) ((QScrollArea *)handle)->takeWidget();
}

bool QScrollArea_widgetResizable(QScrollAreaH handle)
{
	return (bool) ((QScrollArea *)handle)->widgetResizable();
}

void QScrollArea_setWidgetResizable(QScrollAreaH handle, bool resizable)
{
	((QScrollArea *)handle)->setWidgetResizable(resizable);
}

void QScrollArea_sizeHint(QScrollAreaH handle, PSize retval)
{
	*(QSize *)retval = ((QScrollArea *)handle)->sizeHint();
}

bool QScrollArea_focusNextPrevChild(QScrollAreaH handle, bool next)
{
	return (bool) ((QScrollArea *)handle)->focusNextPrevChild(next);
}

unsigned int QScrollArea_alignment(QScrollAreaH handle)
{
	return (unsigned int) ((QScrollArea *)handle)->alignment();
}

void QScrollArea_setAlignment(QScrollAreaH handle, unsigned int AnonParam1)
{
	((QScrollArea *)handle)->setAlignment((Qt::Alignment)AnonParam1);
}

void QScrollArea_ensureVisible(QScrollAreaH handle, int x, int y, int xmargin, int ymargin)
{
	((QScrollArea *)handle)->ensureVisible(x, y, xmargin, ymargin);
}

void QScrollArea_ensureWidgetVisible(QScrollAreaH handle, QWidgetH childWidget, int xmargin, int ymargin)
{
	((QScrollArea *)handle)->ensureWidgetVisible((QWidget*)childWidget, xmargin, ymargin);
}

