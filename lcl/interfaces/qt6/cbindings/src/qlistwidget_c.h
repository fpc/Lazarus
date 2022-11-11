//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLISTWIDGET_C_H
#define QLISTWIDGET_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QListWidgetItemH QListWidgetItem_Create(QListWidgetH view, int type);
C_EXPORT void QListWidgetItem_Destroy(QListWidgetItemH handle);
C_EXPORT QListWidgetItemH QListWidgetItem_Create2(PWideString text, QListWidgetH view, int type);
C_EXPORT QListWidgetItemH QListWidgetItem_Create3(const QIconH icon, PWideString text, QListWidgetH view, int type);
C_EXPORT QListWidgetItemH QListWidgetItem_Create4(const QListWidgetItemH other);
C_EXPORT QListWidgetItemH QListWidgetItem_clone(QListWidgetItemH handle);
C_EXPORT QListWidgetH QListWidgetItem_listWidget(QListWidgetItemH handle);
C_EXPORT void QListWidgetItem_setSelected(QListWidgetItemH handle, bool select);
C_EXPORT bool QListWidgetItem_isSelected(QListWidgetItemH handle);
C_EXPORT void QListWidgetItem_setHidden(QListWidgetItemH handle, bool hide);
C_EXPORT bool QListWidgetItem_isHidden(QListWidgetItemH handle);
C_EXPORT unsigned int QListWidgetItem_flags(QListWidgetItemH handle);
C_EXPORT void QListWidgetItem_setFlags(QListWidgetItemH handle, unsigned int flags);
C_EXPORT void QListWidgetItem_text(QListWidgetItemH handle, PWideString retval);
C_EXPORT void QListWidgetItem_setText(QListWidgetItemH handle, PWideString text);
C_EXPORT void QListWidgetItem_icon(QListWidgetItemH handle, QIconH retval);
C_EXPORT void QListWidgetItem_setIcon(QListWidgetItemH handle, const QIconH icon);
C_EXPORT void QListWidgetItem_statusTip(QListWidgetItemH handle, PWideString retval);
C_EXPORT void QListWidgetItem_setStatusTip(QListWidgetItemH handle, PWideString statusTip);
C_EXPORT void QListWidgetItem_toolTip(QListWidgetItemH handle, PWideString retval);
C_EXPORT void QListWidgetItem_setToolTip(QListWidgetItemH handle, PWideString toolTip);
C_EXPORT void QListWidgetItem_whatsThis(QListWidgetItemH handle, PWideString retval);
C_EXPORT void QListWidgetItem_setWhatsThis(QListWidgetItemH handle, PWideString whatsThis);
C_EXPORT void QListWidgetItem_font(QListWidgetItemH handle, QFontH retval);
C_EXPORT void QListWidgetItem_setFont(QListWidgetItemH handle, const QFontH font);
C_EXPORT int QListWidgetItem_textAlignment(QListWidgetItemH handle);
C_EXPORT void QListWidgetItem_setTextAlignment(QListWidgetItemH handle, int alignment);
C_EXPORT void QListWidgetItem_background(QListWidgetItemH handle, QBrushH retval);
C_EXPORT void QListWidgetItem_setBackground(QListWidgetItemH handle, const QBrushH brush);
C_EXPORT void QListWidgetItem_foreground(QListWidgetItemH handle, QBrushH retval);
C_EXPORT void QListWidgetItem_setForeground(QListWidgetItemH handle, const QBrushH brush);
C_EXPORT Qt::CheckState QListWidgetItem_checkState(QListWidgetItemH handle);
C_EXPORT void QListWidgetItem_setCheckState(QListWidgetItemH handle, Qt::CheckState state);
C_EXPORT void QListWidgetItem_sizeHint(QListWidgetItemH handle, PSize retval);
C_EXPORT void QListWidgetItem_setSizeHint(QListWidgetItemH handle, const QSizeH size);
C_EXPORT void QListWidgetItem_data(QListWidgetItemH handle, QVariantH retval, int role);
C_EXPORT void QListWidgetItem_setData(QListWidgetItemH handle, int role, const QVariantH value);
C_EXPORT void QListWidgetItem_read(QListWidgetItemH handle, QDataStreamH in);
C_EXPORT void QListWidgetItem_write(QListWidgetItemH handle, QDataStreamH out);
C_EXPORT int QListWidgetItem_type(QListWidgetItemH handle);
C_EXPORT QListWidgetH QListWidget_Create(QWidgetH parent);
C_EXPORT void QListWidget_Destroy(QListWidgetH handle);
C_EXPORT QListWidgetItemH QListWidget_item(QListWidgetH handle, int row);
C_EXPORT int QListWidget_row(QListWidgetH handle, const QListWidgetItemH item);
C_EXPORT void QListWidget_insertItem(QListWidgetH handle, int row, QListWidgetItemH item);
C_EXPORT void QListWidget_insertItem2(QListWidgetH handle, int row, PWideString label);
C_EXPORT void QListWidget_insertItems(QListWidgetH handle, int row, const QStringListH labels);
C_EXPORT void QListWidget_addItem(QListWidgetH handle, PWideString label);
C_EXPORT void QListWidget_addItem2(QListWidgetH handle, QListWidgetItemH item);
C_EXPORT void QListWidget_addItems(QListWidgetH handle, const QStringListH labels);
C_EXPORT QListWidgetItemH QListWidget_takeItem(QListWidgetH handle, int row);
C_EXPORT int QListWidget_count(QListWidgetH handle);
C_EXPORT QListWidgetItemH QListWidget_currentItem(QListWidgetH handle);
C_EXPORT void QListWidget_setCurrentItem(QListWidgetH handle, QListWidgetItemH item);
C_EXPORT void QListWidget_setCurrentItem2(QListWidgetH handle, QListWidgetItemH item, unsigned int command);
C_EXPORT int QListWidget_currentRow(QListWidgetH handle);
C_EXPORT void QListWidget_setCurrentRow(QListWidgetH handle, int row);
C_EXPORT void QListWidget_setCurrentRow2(QListWidgetH handle, int row, unsigned int command);
C_EXPORT QListWidgetItemH QListWidget_itemAt(QListWidgetH handle, const QPointH p);
C_EXPORT QListWidgetItemH QListWidget_itemAt2(QListWidgetH handle, int x, int y);
C_EXPORT void QListWidget_visualItemRect(QListWidgetH handle, PRect retval, const QListWidgetItemH item);
C_EXPORT void QListWidget_sortItems(QListWidgetH handle, Qt::SortOrder order);
C_EXPORT void QListWidget_setSortingEnabled(QListWidgetH handle, bool enable);
C_EXPORT bool QListWidget_isSortingEnabled(QListWidgetH handle);
C_EXPORT void QListWidget_editItem(QListWidgetH handle, QListWidgetItemH item);
C_EXPORT void QListWidget_openPersistentEditor(QListWidgetH handle, QListWidgetItemH item);
C_EXPORT void QListWidget_closePersistentEditor(QListWidgetH handle, QListWidgetItemH item);
C_EXPORT QWidgetH QListWidget_itemWidget(QListWidgetH handle, QListWidgetItemH item);
C_EXPORT void QListWidget_setItemWidget(QListWidgetH handle, QListWidgetItemH item, QWidgetH widget);
C_EXPORT void QListWidget_removeItemWidget(QListWidgetH handle, QListWidgetItemH item);
C_EXPORT void QListWidget_selectedItems(QListWidgetH handle, PPtrIntArray retval);
C_EXPORT void QListWidget_findItems(QListWidgetH handle, PPtrIntArray retval, PWideString text, unsigned int flags);
C_EXPORT void QListWidget_scrollToItem(QListWidgetH handle, const QListWidgetItemH item, QAbstractItemView::ScrollHint hint);
C_EXPORT void QListWidget_clear(QListWidgetH handle);
C_EXPORT void QListWidget_indexFromItem(QListWidgetH handle, const QListWidgetItemH item, QModelIndexH retval);
C_EXPORT bool QListWidget_isPersistentEditorOpen(QListWidgetH handle, QListWidgetItemH item);

#endif
