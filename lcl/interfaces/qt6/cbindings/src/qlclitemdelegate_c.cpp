//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlclitemdelegate_c.h"

QLCLItemDelegateH QLCLItemDelegate_Create(QObjectH parent)
{
	return (QLCLItemDelegateH) new QLCLItemDelegate((QObject*)parent);
}

void QLCLItemDelegate_Destroy(QLCLItemDelegateH handle)
{
	delete (QLCLItemDelegate *)handle;
}

void QLCLItemDelegate_override_sizeHint(QLCLItemDelegateH handle, const QOverrideHook hook)
{
	((QLCLItemDelegate *)handle)->override_sizeHint(hook);
}

void QLCLItemDelegate_override_paint(QLCLItemDelegateH handle, const QOverrideHook hook)
{
	((QLCLItemDelegate *)handle)->override_paint(hook);
}

void QLCLItemDelegate_override_createEditor(QLCLItemDelegateH handle, const QOverrideHook hook)
{
	((QLCLItemDelegate *)handle)->override_createEditor(hook);
}

void QLCLItemDelegate_override_setEditorData(QLCLItemDelegateH handle, const QOverrideHook hook)
{
	((QLCLItemDelegate *)handle)->override_setEditorData(hook);
}

void QLCLItemDelegate_override_setModelData(QLCLItemDelegateH handle, const QOverrideHook hook)
{
	((QLCLItemDelegate *)handle)->override_setModelData(hook);
}

void QLCLItemDelegate_override_updateEditorGeometry(QLCLItemDelegateH handle, const QOverrideHook hook)
{
	((QLCLItemDelegate *)handle)->override_updateEditorGeometry(hook);
}

void QLCLItemDelegate_override_editorEvent(QLCLItemDelegateH handle, const QOverrideHook hook)
{
	((QLCLItemDelegate *)handle)->override_editorEvent(hook);
}

bool QLCLItemDelegate_InheritedEditorEvent(QLCLItemDelegateH handle, QEventH event, QAbstractItemModelH model, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	return (bool) ((QLCLItemDelegate *)handle)->InheritedEditorEvent((QEvent*)event, (QAbstractItemModel*)model, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

