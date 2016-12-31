//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qgridlayout_c.h"

QGridLayoutH QGridLayout_Create(QWidgetH parent)
{
	return (QGridLayoutH) new QGridLayout((QWidget*)parent);
}

void QGridLayout_Destroy(QGridLayoutH handle)
{
	delete (QGridLayout *)handle;
}

QGridLayoutH QGridLayout_Create2()
{
	return (QGridLayoutH) new QGridLayout();
}

void QGridLayout_sizeHint(QGridLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QGridLayout *)handle)->sizeHint();
}

void QGridLayout_minimumSize(QGridLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QGridLayout *)handle)->minimumSize();
}

void QGridLayout_maximumSize(QGridLayoutH handle, PSize retval)
{
	*(QSize *)retval = ((QGridLayout *)handle)->maximumSize();
}

void QGridLayout_setHorizontalSpacing(QGridLayoutH handle, int spacing)
{
	((QGridLayout *)handle)->setHorizontalSpacing(spacing);
}

int QGridLayout_horizontalSpacing(QGridLayoutH handle)
{
	return (int) ((QGridLayout *)handle)->horizontalSpacing();
}

void QGridLayout_setVerticalSpacing(QGridLayoutH handle, int spacing)
{
	((QGridLayout *)handle)->setVerticalSpacing(spacing);
}

int QGridLayout_verticalSpacing(QGridLayoutH handle)
{
	return (int) ((QGridLayout *)handle)->verticalSpacing();
}

void QGridLayout_setSpacing(QGridLayoutH handle, int spacing)
{
	((QGridLayout *)handle)->setSpacing(spacing);
}

int QGridLayout_spacing(QGridLayoutH handle)
{
	return (int) ((QGridLayout *)handle)->spacing();
}

void QGridLayout_setRowStretch(QGridLayoutH handle, int row, int stretch)
{
	((QGridLayout *)handle)->setRowStretch(row, stretch);
}

void QGridLayout_setColumnStretch(QGridLayoutH handle, int column, int stretch)
{
	((QGridLayout *)handle)->setColumnStretch(column, stretch);
}

int QGridLayout_rowStretch(QGridLayoutH handle, int row)
{
	return (int) ((QGridLayout *)handle)->rowStretch(row);
}

int QGridLayout_columnStretch(QGridLayoutH handle, int column)
{
	return (int) ((QGridLayout *)handle)->columnStretch(column);
}

void QGridLayout_setRowMinimumHeight(QGridLayoutH handle, int row, int minSize)
{
	((QGridLayout *)handle)->setRowMinimumHeight(row, minSize);
}

void QGridLayout_setColumnMinimumWidth(QGridLayoutH handle, int column, int minSize)
{
	((QGridLayout *)handle)->setColumnMinimumWidth(column, minSize);
}

int QGridLayout_rowMinimumHeight(QGridLayoutH handle, int row)
{
	return (int) ((QGridLayout *)handle)->rowMinimumHeight(row);
}

int QGridLayout_columnMinimumWidth(QGridLayoutH handle, int column)
{
	return (int) ((QGridLayout *)handle)->columnMinimumWidth(column);
}

int QGridLayout_columnCount(QGridLayoutH handle)
{
	return (int) ((QGridLayout *)handle)->columnCount();
}

int QGridLayout_rowCount(QGridLayoutH handle)
{
	return (int) ((QGridLayout *)handle)->rowCount();
}

void QGridLayout_cellRect(QGridLayoutH handle, PRect retval, int row, int column)
{
	QRect t_retval;
	t_retval = ((QGridLayout *)handle)->cellRect(row, column);
	copyQRectToPRect(t_retval, retval);
}

bool QGridLayout_hasHeightForWidth(QGridLayoutH handle)
{
	return (bool) ((QGridLayout *)handle)->hasHeightForWidth();
}

int QGridLayout_heightForWidth(QGridLayoutH handle, int AnonParam1)
{
	return (int) ((QGridLayout *)handle)->heightForWidth(AnonParam1);
}

int QGridLayout_minimumHeightForWidth(QGridLayoutH handle, int AnonParam1)
{
	return (int) ((QGridLayout *)handle)->minimumHeightForWidth(AnonParam1);
}

unsigned int QGridLayout_expandingDirections(QGridLayoutH handle)
{
	return (unsigned int) ((QGridLayout *)handle)->expandingDirections();
}

void QGridLayout_invalidate(QGridLayoutH handle)
{
	((QGridLayout *)handle)->invalidate();
}

void QGridLayout_addWidget(QGridLayoutH handle, QWidgetH w)
{
	((QGridLayout *)handle)->addWidget((QWidget*)w);
}

void QGridLayout_addWidget2(QGridLayoutH handle, QWidgetH AnonParam1, int row, int column, unsigned int AnonParam4)
{
	((QGridLayout *)handle)->addWidget((QWidget*)AnonParam1, row, column, (Qt::Alignment)AnonParam4);
}

void QGridLayout_addWidget3(QGridLayoutH handle, QWidgetH AnonParam1, int row, int column, int rowSpan, int columnSpan, unsigned int AnonParam6)
{
	((QGridLayout *)handle)->addWidget((QWidget*)AnonParam1, row, column, rowSpan, columnSpan, (Qt::Alignment)AnonParam6);
}

void QGridLayout_addLayout(QGridLayoutH handle, QLayoutH AnonParam1, int row, int column, unsigned int AnonParam4)
{
	((QGridLayout *)handle)->addLayout((QLayout*)AnonParam1, row, column, (Qt::Alignment)AnonParam4);
}

void QGridLayout_addLayout2(QGridLayoutH handle, QLayoutH AnonParam1, int row, int column, int rowSpan, int columnSpan, unsigned int AnonParam6)
{
	((QGridLayout *)handle)->addLayout((QLayout*)AnonParam1, row, column, rowSpan, columnSpan, (Qt::Alignment)AnonParam6);
}

void QGridLayout_setOriginCorner(QGridLayoutH handle, Qt::Corner AnonParam1)
{
	((QGridLayout *)handle)->setOriginCorner(AnonParam1);
}

Qt::Corner QGridLayout_originCorner(QGridLayoutH handle)
{
	return (Qt::Corner) ((QGridLayout *)handle)->originCorner();
}

QLayoutItemH QGridLayout_itemAt(QGridLayoutH handle, int index)
{
	return (QLayoutItemH) ((QGridLayout *)handle)->itemAt(index);
}

QLayoutItemH QGridLayout_itemAtPosition(QGridLayoutH handle, int row, int column)
{
	return (QLayoutItemH) ((QGridLayout *)handle)->itemAtPosition(row, column);
}

QLayoutItemH QGridLayout_takeAt(QGridLayoutH handle, int index)
{
	return (QLayoutItemH) ((QGridLayout *)handle)->takeAt(index);
}

int QGridLayout_count(QGridLayoutH handle)
{
	return (int) ((QGridLayout *)handle)->count();
}

void QGridLayout_setGeometry(QGridLayoutH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QGridLayout *)handle)->setGeometry(t_AnonParam1);
}

void QGridLayout_addItem(QGridLayoutH handle, QLayoutItemH item, int row, int column, int rowSpan, int columnSpan, unsigned int AnonParam6)
{
	((QGridLayout *)handle)->addItem((QLayoutItem*)item, row, column, rowSpan, columnSpan, (Qt::Alignment)AnonParam6);
}

void QGridLayout_setDefaultPositioning(QGridLayoutH handle, int n, Qt::Orientation orient)
{
	((QGridLayout *)handle)->setDefaultPositioning(n, orient);
}

void QGridLayout_getItemPosition(QGridLayoutH handle, int idx, int* row, int* column, int* rowSpan, int* columnSpan)
{
	((QGridLayout *)handle)->getItemPosition(idx, row, column, rowSpan, columnSpan);
}

