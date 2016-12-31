//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlayoutitem_c.h"

void QLayoutItem_sizeHint(QLayoutItemH handle, PSize retval)
{
	*(QSize *)retval = ((QLayoutItem *)handle)->sizeHint();
}

void QLayoutItem_minimumSize(QLayoutItemH handle, PSize retval)
{
	*(QSize *)retval = ((QLayoutItem *)handle)->minimumSize();
}

void QLayoutItem_maximumSize(QLayoutItemH handle, PSize retval)
{
	*(QSize *)retval = ((QLayoutItem *)handle)->maximumSize();
}

unsigned int QLayoutItem_expandingDirections(QLayoutItemH handle)
{
	return (unsigned int) ((QLayoutItem *)handle)->expandingDirections();
}

void QLayoutItem_setGeometry(QLayoutItemH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QLayoutItem *)handle)->setGeometry(t_AnonParam1);
}

void QLayoutItem_geometry(QLayoutItemH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QLayoutItem *)handle)->geometry();
	copyQRectToPRect(t_retval, retval);
}

bool QLayoutItem_isEmpty(QLayoutItemH handle)
{
	return (bool) ((QLayoutItem *)handle)->isEmpty();
}

bool QLayoutItem_hasHeightForWidth(QLayoutItemH handle)
{
	return (bool) ((QLayoutItem *)handle)->hasHeightForWidth();
}

int QLayoutItem_heightForWidth(QLayoutItemH handle, int AnonParam1)
{
	return (int) ((QLayoutItem *)handle)->heightForWidth(AnonParam1);
}

int QLayoutItem_minimumHeightForWidth(QLayoutItemH handle, int AnonParam1)
{
	return (int) ((QLayoutItem *)handle)->minimumHeightForWidth(AnonParam1);
}

void QLayoutItem_invalidate(QLayoutItemH handle)
{
	((QLayoutItem *)handle)->invalidate();
}

QWidgetH QLayoutItem_widget(QLayoutItemH handle)
{
	return (QWidgetH) ((QLayoutItem *)handle)->widget();
}

QLayoutH QLayoutItem_layout(QLayoutItemH handle)
{
	return (QLayoutH) ((QLayoutItem *)handle)->layout();
}

QSpacerItemH QLayoutItem_spacerItem(QLayoutItemH handle)
{
	return (QSpacerItemH) ((QLayoutItem *)handle)->spacerItem();
}

unsigned int QLayoutItem_alignment(QLayoutItemH handle)
{
	return (unsigned int) ((QLayoutItem *)handle)->alignment();
}

void QLayoutItem_setAlignment(QLayoutItemH handle, unsigned int a)
{
	((QLayoutItem *)handle)->setAlignment((Qt::Alignment)a);
}

unsigned int QLayoutItem_controlTypes(QLayoutItemH handle)
{
	return (unsigned int) ((QLayoutItem *)handle)->controlTypes();
}

QSpacerItemH QSpacerItem_Create(int w, int h, QSizePolicy::Policy hData, QSizePolicy::Policy vData)
{
	return (QSpacerItemH) new QSpacerItem(w, h, hData, vData);
}

void QSpacerItem_Destroy(QSpacerItemH handle)
{
	delete (QSpacerItem *)handle;
}

void QSpacerItem_changeSize(QSpacerItemH handle, int w, int h, QSizePolicy::Policy hData, QSizePolicy::Policy vData)
{
	((QSpacerItem *)handle)->changeSize(w, h, hData, vData);
}

void QSpacerItem_sizeHint(QSpacerItemH handle, PSize retval)
{
	*(QSize *)retval = ((QSpacerItem *)handle)->sizeHint();
}

void QSpacerItem_minimumSize(QSpacerItemH handle, PSize retval)
{
	*(QSize *)retval = ((QSpacerItem *)handle)->minimumSize();
}

void QSpacerItem_maximumSize(QSpacerItemH handle, PSize retval)
{
	*(QSize *)retval = ((QSpacerItem *)handle)->maximumSize();
}

unsigned int QSpacerItem_expandingDirections(QSpacerItemH handle)
{
	return (unsigned int) ((QSpacerItem *)handle)->expandingDirections();
}

bool QSpacerItem_isEmpty(QSpacerItemH handle)
{
	return (bool) ((QSpacerItem *)handle)->isEmpty();
}

void QSpacerItem_setGeometry(QSpacerItemH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QSpacerItem *)handle)->setGeometry(t_AnonParam1);
}

void QSpacerItem_geometry(QSpacerItemH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QSpacerItem *)handle)->geometry();
	copyQRectToPRect(t_retval, retval);
}

QSpacerItemH QSpacerItem_spacerItem(QSpacerItemH handle)
{
	return (QSpacerItemH) ((QSpacerItem *)handle)->spacerItem();
}

QWidgetItemH QWidgetItem_Create(QWidgetH w)
{
	return (QWidgetItemH) new QWidgetItem((QWidget*)w);
}

void QWidgetItem_Destroy(QWidgetItemH handle)
{
	delete (QWidgetItem *)handle;
}

void QWidgetItem_sizeHint(QWidgetItemH handle, PSize retval)
{
	*(QSize *)retval = ((QWidgetItem *)handle)->sizeHint();
}

void QWidgetItem_minimumSize(QWidgetItemH handle, PSize retval)
{
	*(QSize *)retval = ((QWidgetItem *)handle)->minimumSize();
}

void QWidgetItem_maximumSize(QWidgetItemH handle, PSize retval)
{
	*(QSize *)retval = ((QWidgetItem *)handle)->maximumSize();
}

unsigned int QWidgetItem_expandingDirections(QWidgetItemH handle)
{
	return (unsigned int) ((QWidgetItem *)handle)->expandingDirections();
}

bool QWidgetItem_isEmpty(QWidgetItemH handle)
{
	return (bool) ((QWidgetItem *)handle)->isEmpty();
}

void QWidgetItem_setGeometry(QWidgetItemH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QWidgetItem *)handle)->setGeometry(t_AnonParam1);
}

void QWidgetItem_geometry(QWidgetItemH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWidgetItem *)handle)->geometry();
	copyQRectToPRect(t_retval, retval);
}

QWidgetH QWidgetItem_widget(QWidgetItemH handle)
{
	return (QWidgetH) ((QWidgetItem *)handle)->widget();
}

bool QWidgetItem_hasHeightForWidth(QWidgetItemH handle)
{
	return (bool) ((QWidgetItem *)handle)->hasHeightForWidth();
}

int QWidgetItem_heightForWidth(QWidgetItemH handle, int AnonParam1)
{
	return (int) ((QWidgetItem *)handle)->heightForWidth(AnonParam1);
}

unsigned int QWidgetItem_controlTypes(QWidgetItemH handle)
{
	return (unsigned int) ((QWidgetItem *)handle)->controlTypes();
}

