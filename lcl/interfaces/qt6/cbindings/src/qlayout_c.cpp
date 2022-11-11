//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlayout_c.h"

int QLayout_spacing(QLayoutH handle)
{
	return (int) ((QLayout *)handle)->spacing();
}

void QLayout_setSpacing(QLayoutH handle, int AnonParam1)
{
	((QLayout *)handle)->setSpacing(AnonParam1);
}

void QLayout_setContentsMargins(QLayoutH handle, int left, int top, int right, int bottom)
{
	((QLayout *)handle)->setContentsMargins(left, top, right, bottom);
}

void QLayout_setContentsMargins2(QLayoutH handle, const QMarginsH margins)
{
	((QLayout *)handle)->setContentsMargins(*(const QMargins*)margins);
}

void QLayout_getContentsMargins(QLayoutH handle, int* left, int* top, int* right, int* bottom)
{
	((QLayout *)handle)->getContentsMargins(left, top, right, bottom);
}

void QLayout_contentsMargins(QLayoutH handle, QMarginsH retval)
{
	*(QMargins *)retval = ((QLayout *)handle)->contentsMargins();
}

void QLayout_contentsRect(QLayoutH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QLayout *)handle)->contentsRect();
	copyQRectToPRect(t_retval, retval);
}

bool QLayout_setAlignment(QLayoutH handle, QWidgetH w, unsigned int alignment)
{
	return (bool) ((QLayout *)handle)->setAlignment((QWidget*)w, (Qt::Alignment)alignment);
}

bool QLayout_setAlignment2(QLayoutH handle, QLayoutH l, unsigned int alignment)
{
	return (bool) ((QLayout *)handle)->setAlignment((QLayout*)l, (Qt::Alignment)alignment);
}

void QLayout_setSizeConstraint(QLayoutH handle, QLayout::SizeConstraint AnonParam1)
{
	((QLayout *)handle)->setSizeConstraint(AnonParam1);
}

QLayout::SizeConstraint QLayout_sizeConstraint(QLayoutH handle)
{
	return (QLayout::SizeConstraint) ((QLayout *)handle)->sizeConstraint();
}

void QLayout_setMenuBar(QLayoutH handle, QWidgetH w)
{
	((QLayout *)handle)->setMenuBar((QWidget*)w);
}

QWidgetH QLayout_menuBar(QLayoutH handle)
{
	return (QWidgetH) ((QLayout *)handle)->menuBar();
}

QWidgetH QLayout_parentWidget(QLayoutH handle)
{
	return (QWidgetH) ((QLayout *)handle)->parentWidget();
}

void QLayout_invalidate(QLayoutH handle)
{
	((QLayout *)handle)->invalidate();
}

void QLayout_geometry(QLayoutH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QLayout *)handle)->geometry();
	copyQRectToPRect(t_retval, retval);
}

bool QLayout_activate(QLayoutH handle)
{
	return (bool) ((QLayout *)handle)->activate();
}

void QLayout_update(QLayoutH handle)
{
	((QLayout *)handle)->update();
}

void QLayout_addWidget(QLayoutH handle, QWidgetH w)
{
	((QLayout *)handle)->addWidget((QWidget*)w);
}

void QLayout_addItem(QLayoutH handle, QLayoutItemH AnonParam1)
{
	((QLayout *)handle)->addItem((QLayoutItem*)AnonParam1);
}

void QLayout_removeWidget(QLayoutH handle, QWidgetH w)
{
	((QLayout *)handle)->removeWidget((QWidget*)w);
}

void QLayout_removeItem(QLayoutH handle, QLayoutItemH AnonParam1)
{
	((QLayout *)handle)->removeItem((QLayoutItem*)AnonParam1);
}

unsigned int QLayout_expandingDirections(QLayoutH handle)
{
	return (unsigned int) ((QLayout *)handle)->expandingDirections();
}

void QLayout_minimumSize(QLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QLayout *)handle)->minimumSize();
}

void QLayout_maximumSize(QLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QLayout *)handle)->maximumSize();
}

void QLayout_setGeometry(QLayoutH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QLayout *)handle)->setGeometry(t_AnonParam1);
}

QLayoutItemH QLayout_itemAt(QLayoutH handle, int index)
{
	return (QLayoutItemH) ((QLayout *)handle)->itemAt(index);
}

QLayoutItemH QLayout_takeAt(QLayoutH handle, int index)
{
	return (QLayoutItemH) ((QLayout *)handle)->takeAt(index);
}

int QLayout_indexOf(QLayoutH handle, QWidgetH AnonParam1)
{
	return (int) ((QLayout *)handle)->indexOf((QWidget*)AnonParam1);
}

int QLayout_count(QLayoutH handle)
{
	return (int) ((QLayout *)handle)->count();
}

bool QLayout_isEmpty(QLayoutH handle)
{
	return (bool) ((QLayout *)handle)->isEmpty();
}

unsigned int QLayout_controlTypes(QLayoutH handle)
{
	return (unsigned int) ((QLayout *)handle)->controlTypes();
}

int QLayout_totalHeightForWidth(QLayoutH handle, int w)
{
	return (int) ((QLayout *)handle)->totalHeightForWidth(w);
}

void QLayout_totalMinimumSize(QLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QLayout *)handle)->totalMinimumSize();
}

void QLayout_totalMaximumSize(QLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QLayout *)handle)->totalMaximumSize();
}

void QLayout_totalSizeHint(QLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QLayout *)handle)->totalSizeHint();
}

QLayoutH QLayout_layout(QLayoutH handle)
{
	return (QLayoutH) ((QLayout *)handle)->layout();
}

void QLayout_setEnabled(QLayoutH handle, bool AnonParam1)
{
	((QLayout *)handle)->setEnabled(AnonParam1);
}

bool QLayout_isEnabled(QLayoutH handle)
{
	return (bool) ((QLayout *)handle)->isEnabled();
}

void QLayout_closestAcceptableSize(PSize retval, const QWidgetH w, const QSizeH s)
{
	*(QSize *)retval = QLayout::closestAcceptableSize((const QWidget*)w, *(const QSize*)s);
}

QLayoutItemH QLayout_to_QLayoutItem(QLayoutH handle)
{
	return (QLayoutItemH)(QLayoutItem *)(QLayout *)handle;
}

