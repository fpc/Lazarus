//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsplitter_c.h"

QSplitterH QSplitter_Create(QWidgetH parent)
{
	return (QSplitterH) new QSplitter((QWidget*)parent);
}

void QSplitter_Destroy(QSplitterH handle)
{
	delete (QSplitter *)handle;
}

QSplitterH QSplitter_Create2(Qt::Orientation AnonParam1, QWidgetH parent)
{
	return (QSplitterH) new QSplitter(AnonParam1, (QWidget*)parent);
}

void QSplitter_addWidget(QSplitterH handle, QWidgetH widget)
{
	((QSplitter *)handle)->addWidget((QWidget*)widget);
}

void QSplitter_insertWidget(QSplitterH handle, int index, QWidgetH widget)
{
	((QSplitter *)handle)->insertWidget(index, (QWidget*)widget);
}

void QSplitter_setOrientation(QSplitterH handle, Qt::Orientation AnonParam1)
{
	((QSplitter *)handle)->setOrientation(AnonParam1);
}

Qt::Orientation QSplitter_orientation(QSplitterH handle)
{
	return (Qt::Orientation) ((QSplitter *)handle)->orientation();
}

void QSplitter_setChildrenCollapsible(QSplitterH handle, bool AnonParam1)
{
	((QSplitter *)handle)->setChildrenCollapsible(AnonParam1);
}

bool QSplitter_childrenCollapsible(QSplitterH handle)
{
	return (bool) ((QSplitter *)handle)->childrenCollapsible();
}

void QSplitter_setCollapsible(QSplitterH handle, int index, bool AnonParam2)
{
	((QSplitter *)handle)->setCollapsible(index, AnonParam2);
}

bool QSplitter_isCollapsible(QSplitterH handle, int index)
{
	return (bool) ((QSplitter *)handle)->isCollapsible(index);
}

void QSplitter_setOpaqueResize(QSplitterH handle, bool opaque)
{
	((QSplitter *)handle)->setOpaqueResize(opaque);
}

bool QSplitter_opaqueResize(QSplitterH handle)
{
	return (bool) ((QSplitter *)handle)->opaqueResize();
}

void QSplitter_refresh(QSplitterH handle)
{
	((QSplitter *)handle)->refresh();
}

void QSplitter_sizeHint(QSplitterH handle, PSize retval)
{
	*(QSize *)retval = ((QSplitter *)handle)->sizeHint();
}

void QSplitter_minimumSizeHint(QSplitterH handle, PSize retval)
{
	*(QSize *)retval = ((QSplitter *)handle)->minimumSizeHint();
}

void QSplitter_sizes(QSplitterH handle, PPtrIntArray retval)
{
	QList<int> t_retval;
	t_retval = ((QSplitter *)handle)->sizes();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QSplitter_setSizes(QSplitterH handle, PPtrIntArray list)
{
	QList<int> t_list;
	copyPtrIntArrayToQListTemplate(list, t_list);
	((QSplitter *)handle)->setSizes(t_list);
}

void QSplitter_saveState(QSplitterH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QSplitter *)handle)->saveState();
}

bool QSplitter_restoreState(QSplitterH handle, const QByteArrayH state)
{
	return (bool) ((QSplitter *)handle)->restoreState(*(const QByteArray*)state);
}

int QSplitter_handleWidth(QSplitterH handle)
{
	return (int) ((QSplitter *)handle)->handleWidth();
}

void QSplitter_setHandleWidth(QSplitterH handle, int AnonParam1)
{
	((QSplitter *)handle)->setHandleWidth(AnonParam1);
}

int QSplitter_indexOf(QSplitterH handle, QWidgetH w)
{
	return (int) ((QSplitter *)handle)->indexOf((QWidget*)w);
}

QWidgetH QSplitter_widget(QSplitterH handle, int index)
{
	return (QWidgetH) ((QSplitter *)handle)->widget(index);
}

int QSplitter_count(QSplitterH handle)
{
	return (int) ((QSplitter *)handle)->count();
}

void QSplitter_getRange(QSplitterH handle, int index, int* AnonParam2, int* AnonParam3)
{
	((QSplitter *)handle)->getRange(index, AnonParam2, AnonParam3);
}

QSplitterHandleH QSplitter_handle(QSplitterH handle, int index)
{
	return (QSplitterHandleH) ((QSplitter *)handle)->handle(index);
}

void QSplitter_setStretchFactor(QSplitterH handle, int index, int stretch)
{
	((QSplitter *)handle)->setStretchFactor(index, stretch);
}

QSplitterHandleH QSplitterHandle_Create(Qt::Orientation o, QSplitterH parent)
{
	return (QSplitterHandleH) new QSplitterHandle(o, (QSplitter*)parent);
}

void QSplitterHandle_Destroy(QSplitterHandleH handle)
{
	delete (QSplitterHandle *)handle;
}

void QSplitterHandle_setOrientation(QSplitterHandleH handle, Qt::Orientation o)
{
	((QSplitterHandle *)handle)->setOrientation(o);
}

Qt::Orientation QSplitterHandle_orientation(QSplitterHandleH handle)
{
	return (Qt::Orientation) ((QSplitterHandle *)handle)->orientation();
}

bool QSplitterHandle_opaqueResize(QSplitterHandleH handle)
{
	return (bool) ((QSplitterHandle *)handle)->opaqueResize();
}

QSplitterH QSplitterHandle_splitter(QSplitterHandleH handle)
{
	return (QSplitterH) ((QSplitterHandle *)handle)->splitter();
}

void QSplitterHandle_sizeHint(QSplitterHandleH handle, PSize retval)
{
	*(QSize *)retval = ((QSplitterHandle *)handle)->sizeHint();
}

