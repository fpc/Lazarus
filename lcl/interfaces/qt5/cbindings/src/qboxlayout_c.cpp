//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qboxlayout_c.h"

QBoxLayoutH QBoxLayout_Create(QBoxLayout::Direction AnonParam1, QWidgetH parent)
{
	return (QBoxLayoutH) new QBoxLayout(AnonParam1, (QWidget*)parent);
}

void QBoxLayout_Destroy(QBoxLayoutH handle)
{
	delete (QBoxLayout *)handle;
}

QBoxLayout::Direction QBoxLayout_direction(QBoxLayoutH handle)
{
	return (QBoxLayout::Direction) ((QBoxLayout *)handle)->direction();
}

void QBoxLayout_setDirection(QBoxLayoutH handle, QBoxLayout::Direction AnonParam1)
{
	((QBoxLayout *)handle)->setDirection(AnonParam1);
}

void QBoxLayout_addSpacing(QBoxLayoutH handle, int size)
{
	((QBoxLayout *)handle)->addSpacing(size);
}

void QBoxLayout_addStretch(QBoxLayoutH handle, int stretch)
{
	((QBoxLayout *)handle)->addStretch(stretch);
}

void QBoxLayout_addSpacerItem(QBoxLayoutH handle, QSpacerItemH spacerItem)
{
	((QBoxLayout *)handle)->addSpacerItem((QSpacerItem*)spacerItem);
}

void QBoxLayout_addWidget(QBoxLayoutH handle, QWidgetH AnonParam1, int stretch, unsigned int alignment)
{
	((QBoxLayout *)handle)->addWidget((QWidget*)AnonParam1, stretch, (Qt::Alignment)alignment);
}

void QBoxLayout_addLayout(QBoxLayoutH handle, QLayoutH layout, int stretch)
{
	((QBoxLayout *)handle)->addLayout((QLayout*)layout, stretch);
}

void QBoxLayout_addStrut(QBoxLayoutH handle, int AnonParam1)
{
	((QBoxLayout *)handle)->addStrut(AnonParam1);
}

void QBoxLayout_addItem(QBoxLayoutH handle, QLayoutItemH AnonParam1)
{
	((QBoxLayout *)handle)->addItem((QLayoutItem*)AnonParam1);
}

void QBoxLayout_insertSpacing(QBoxLayoutH handle, int index, int size)
{
	((QBoxLayout *)handle)->insertSpacing(index, size);
}

void QBoxLayout_insertStretch(QBoxLayoutH handle, int index, int stretch)
{
	((QBoxLayout *)handle)->insertStretch(index, stretch);
}

void QBoxLayout_insertSpacerItem(QBoxLayoutH handle, int index, QSpacerItemH spacerItem)
{
	((QBoxLayout *)handle)->insertSpacerItem(index, (QSpacerItem*)spacerItem);
}

void QBoxLayout_insertWidget(QBoxLayoutH handle, int index, QWidgetH widget, int stretch, unsigned int alignment)
{
	((QBoxLayout *)handle)->insertWidget(index, (QWidget*)widget, stretch, (Qt::Alignment)alignment);
}

void QBoxLayout_insertLayout(QBoxLayoutH handle, int index, QLayoutH layout, int stretch)
{
	((QBoxLayout *)handle)->insertLayout(index, (QLayout*)layout, stretch);
}

void QBoxLayout_insertItem(QBoxLayoutH handle, int index, QLayoutItemH AnonParam2)
{
	((QBoxLayout *)handle)->insertItem(index, (QLayoutItem*)AnonParam2);
}

int QBoxLayout_spacing(QBoxLayoutH handle)
{
	return (int) ((QBoxLayout *)handle)->spacing();
}

void QBoxLayout_setSpacing(QBoxLayoutH handle, int spacing)
{
	((QBoxLayout *)handle)->setSpacing(spacing);
}

bool QBoxLayout_setStretchFactor(QBoxLayoutH handle, QWidgetH w, int stretch)
{
	return (bool) ((QBoxLayout *)handle)->setStretchFactor((QWidget*)w, stretch);
}

bool QBoxLayout_setStretchFactor2(QBoxLayoutH handle, QLayoutH l, int stretch)
{
	return (bool) ((QBoxLayout *)handle)->setStretchFactor((QLayout*)l, stretch);
}

void QBoxLayout_setStretch(QBoxLayoutH handle, int index, int stretch)
{
	((QBoxLayout *)handle)->setStretch(index, stretch);
}

int QBoxLayout_stretch(QBoxLayoutH handle, int index)
{
	return (int) ((QBoxLayout *)handle)->stretch(index);
}

void QBoxLayout_sizeHint(QBoxLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QBoxLayout *)handle)->sizeHint();
}

void QBoxLayout_minimumSize(QBoxLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QBoxLayout *)handle)->minimumSize();
}

void QBoxLayout_maximumSize(QBoxLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QBoxLayout *)handle)->maximumSize();
}

bool QBoxLayout_hasHeightForWidth(QBoxLayoutH handle)
{
	return (bool) ((QBoxLayout *)handle)->hasHeightForWidth();
}

int QBoxLayout_heightForWidth(QBoxLayoutH handle, int AnonParam1)
{
	return (int) ((QBoxLayout *)handle)->heightForWidth(AnonParam1);
}

int QBoxLayout_minimumHeightForWidth(QBoxLayoutH handle, int AnonParam1)
{
	return (int) ((QBoxLayout *)handle)->minimumHeightForWidth(AnonParam1);
}

unsigned int QBoxLayout_expandingDirections(QBoxLayoutH handle)
{
	return (unsigned int) ((QBoxLayout *)handle)->expandingDirections();
}

void QBoxLayout_invalidate(QBoxLayoutH handle)
{
	((QBoxLayout *)handle)->invalidate();
}

QLayoutItemH QBoxLayout_itemAt(QBoxLayoutH handle, int AnonParam1)
{
	return (QLayoutItemH) ((QBoxLayout *)handle)->itemAt(AnonParam1);
}

QLayoutItemH QBoxLayout_takeAt(QBoxLayoutH handle, int AnonParam1)
{
	return (QLayoutItemH) ((QBoxLayout *)handle)->takeAt(AnonParam1);
}

int QBoxLayout_count(QBoxLayoutH handle)
{
	return (int) ((QBoxLayout *)handle)->count();
}

void QBoxLayout_setGeometry(QBoxLayoutH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QBoxLayout *)handle)->setGeometry(t_AnonParam1);
}

QHBoxLayoutH QHBoxLayout_Create()
{
	return (QHBoxLayoutH) new QHBoxLayout();
}

void QHBoxLayout_Destroy(QHBoxLayoutH handle)
{
	delete (QHBoxLayout *)handle;
}

QHBoxLayoutH QHBoxLayout_Create2(QWidgetH parent)
{
	return (QHBoxLayoutH) new QHBoxLayout((QWidget*)parent);
}

QVBoxLayoutH QVBoxLayout_Create()
{
	return (QVBoxLayoutH) new QVBoxLayout();
}

void QVBoxLayout_Destroy(QVBoxLayoutH handle)
{
	delete (QVBoxLayout *)handle;
}

QVBoxLayoutH QVBoxLayout_Create2(QWidgetH parent)
{
	return (QVBoxLayoutH) new QVBoxLayout((QWidget*)parent);
}

