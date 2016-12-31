//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstackedlayout_c.h"

QStackedLayoutH QStackedLayout_Create()
{
	return (QStackedLayoutH) new QStackedLayout();
}

void QStackedLayout_Destroy(QStackedLayoutH handle)
{
	delete (QStackedLayout *)handle;
}

QStackedLayoutH QStackedLayout_Create2(QWidgetH parent)
{
	return (QStackedLayoutH) new QStackedLayout((QWidget*)parent);
}

QStackedLayoutH QStackedLayout_Create3(QLayoutH parentLayout)
{
	return (QStackedLayoutH) new QStackedLayout((QLayout*)parentLayout);
}

int QStackedLayout_addWidget(QStackedLayoutH handle, QWidgetH w)
{
	return (int) ((QStackedLayout *)handle)->addWidget((QWidget*)w);
}

int QStackedLayout_insertWidget(QStackedLayoutH handle, int index, QWidgetH w)
{
	return (int) ((QStackedLayout *)handle)->insertWidget(index, (QWidget*)w);
}

QWidgetH QStackedLayout_currentWidget(QStackedLayoutH handle)
{
	return (QWidgetH) ((QStackedLayout *)handle)->currentWidget();
}

int QStackedLayout_currentIndex(QStackedLayoutH handle)
{
	return (int) ((QStackedLayout *)handle)->currentIndex();
}

QWidgetH QStackedLayout_widget(QStackedLayoutH handle, int AnonParam1)
{
	return (QWidgetH) ((QStackedLayout *)handle)->widget(AnonParam1);
}

int QStackedLayout_count(QStackedLayoutH handle)
{
	return (int) ((QStackedLayout *)handle)->count();
}

QStackedLayout::StackingMode QStackedLayout_stackingMode(QStackedLayoutH handle)
{
	return (QStackedLayout::StackingMode) ((QStackedLayout *)handle)->stackingMode();
}

void QStackedLayout_setStackingMode(QStackedLayoutH handle, QStackedLayout::StackingMode stackingMode)
{
	((QStackedLayout *)handle)->setStackingMode(stackingMode);
}

void QStackedLayout_addItem(QStackedLayoutH handle, QLayoutItemH item)
{
	((QStackedLayout *)handle)->addItem((QLayoutItem*)item);
}

void QStackedLayout_sizeHint(QStackedLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QStackedLayout *)handle)->sizeHint();
}

void QStackedLayout_minimumSize(QStackedLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QStackedLayout *)handle)->minimumSize();
}

QLayoutItemH QStackedLayout_itemAt(QStackedLayoutH handle, int AnonParam1)
{
	return (QLayoutItemH) ((QStackedLayout *)handle)->itemAt(AnonParam1);
}

QLayoutItemH QStackedLayout_takeAt(QStackedLayoutH handle, int AnonParam1)
{
	return (QLayoutItemH) ((QStackedLayout *)handle)->takeAt(AnonParam1);
}

void QStackedLayout_setGeometry(QStackedLayoutH handle, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QStackedLayout *)handle)->setGeometry(t_rect);
}

bool QStackedLayout_hasHeightForWidth(QStackedLayoutH handle)
{
	return (bool) ((QStackedLayout *)handle)->hasHeightForWidth();
}

int QStackedLayout_heightForWidth(QStackedLayoutH handle, int width)
{
	return (int) ((QStackedLayout *)handle)->heightForWidth(width);
}

void QStackedLayout_setCurrentIndex(QStackedLayoutH handle, int index)
{
	((QStackedLayout *)handle)->setCurrentIndex(index);
}

void QStackedLayout_setCurrentWidget(QStackedLayoutH handle, QWidgetH w)
{
	((QStackedLayout *)handle)->setCurrentWidget((QWidget*)w);
}

