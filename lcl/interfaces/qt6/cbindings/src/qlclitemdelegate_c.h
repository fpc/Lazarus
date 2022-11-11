//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCLITEMDELEGATE_C_H
#define QLCLITEMDELEGATE_C_H

#include "qlclitemdelegate.h"
#include "pascalbind.h"

C_EXPORT QLCLItemDelegateH QLCLItemDelegate_Create(QObjectH parent);
C_EXPORT void QLCLItemDelegate_Destroy(QLCLItemDelegateH handle);
C_EXPORT void QLCLItemDelegate_override_sizeHint(QLCLItemDelegateH handle, const QOverrideHook hook);
C_EXPORT void QLCLItemDelegate_override_paint(QLCLItemDelegateH handle, const QOverrideHook hook);
C_EXPORT void QLCLItemDelegate_override_createEditor(QLCLItemDelegateH handle, const QOverrideHook hook);
C_EXPORT void QLCLItemDelegate_override_setEditorData(QLCLItemDelegateH handle, const QOverrideHook hook);
C_EXPORT void QLCLItemDelegate_override_setModelData(QLCLItemDelegateH handle, const QOverrideHook hook);
C_EXPORT void QLCLItemDelegate_override_updateEditorGeometry(QLCLItemDelegateH handle, const QOverrideHook hook);
C_EXPORT void QLCLItemDelegate_override_editorEvent(QLCLItemDelegateH handle, const QOverrideHook hook);
C_EXPORT bool QLCLItemDelegate_InheritedEditorEvent(QLCLItemDelegateH handle, QEventH event, QAbstractItemModelH model, const QStyleOptionViewItemH option, const QModelIndexH index);

#endif
