//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstackedwidget_c.h"

QStackedWidgetH QStackedWidget_Create(QWidgetH parent)
{
	return (QStackedWidgetH) new QStackedWidget((QWidget*)parent);
}

void QStackedWidget_Destroy(QStackedWidgetH handle)
{
	delete (QStackedWidget *)handle;
}

int QStackedWidget_addWidget(QStackedWidgetH handle, QWidgetH w)
{
	return (int) ((QStackedWidget *)handle)->addWidget((QWidget*)w);
}

int QStackedWidget_insertWidget(QStackedWidgetH handle, int index, QWidgetH w)
{
	return (int) ((QStackedWidget *)handle)->insertWidget(index, (QWidget*)w);
}

void QStackedWidget_removeWidget(QStackedWidgetH handle, QWidgetH w)
{
	((QStackedWidget *)handle)->removeWidget((QWidget*)w);
}

QWidgetH QStackedWidget_currentWidget(QStackedWidgetH handle)
{
	return (QWidgetH) ((QStackedWidget *)handle)->currentWidget();
}

int QStackedWidget_currentIndex(QStackedWidgetH handle)
{
	return (int) ((QStackedWidget *)handle)->currentIndex();
}

int QStackedWidget_indexOf(QStackedWidgetH handle, QWidgetH AnonParam1)
{
	return (int) ((QStackedWidget *)handle)->indexOf((QWidget*)AnonParam1);
}

QWidgetH QStackedWidget_widget(QStackedWidgetH handle, int AnonParam1)
{
	return (QWidgetH) ((QStackedWidget *)handle)->widget(AnonParam1);
}

int QStackedWidget_count(QStackedWidgetH handle)
{
	return (int) ((QStackedWidget *)handle)->count();
}

void QStackedWidget_setCurrentIndex(QStackedWidgetH handle, int index)
{
	((QStackedWidget *)handle)->setCurrentIndex(index);
}

void QStackedWidget_setCurrentWidget(QStackedWidgetH handle, QWidgetH w)
{
	((QStackedWidget *)handle)->setCurrentWidget((QWidget*)w);
}

