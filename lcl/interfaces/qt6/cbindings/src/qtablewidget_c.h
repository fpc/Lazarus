//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABLEWIDGET_C_H
#define QTABLEWIDGET_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QTableWidgetSelectionRangeH QTableWidgetSelectionRange_Create();
C_EXPORT void QTableWidgetSelectionRange_Destroy(QTableWidgetSelectionRangeH handle);
C_EXPORT QTableWidgetSelectionRangeH QTableWidgetSelectionRange_Create2(int top, int left, int bottom, int right);
C_EXPORT QTableWidgetSelectionRangeH QTableWidgetSelectionRange_Create3(const QTableWidgetSelectionRangeH other);
C_EXPORT int QTableWidgetSelectionRange_topRow(QTableWidgetSelectionRangeH handle);
C_EXPORT int QTableWidgetSelectionRange_bottomRow(QTableWidgetSelectionRangeH handle);
C_EXPORT int QTableWidgetSelectionRange_leftColumn(QTableWidgetSelectionRangeH handle);
C_EXPORT int QTableWidgetSelectionRange_rightColumn(QTableWidgetSelectionRangeH handle);
C_EXPORT int QTableWidgetSelectionRange_rowCount(QTableWidgetSelectionRangeH handle);
C_EXPORT int QTableWidgetSelectionRange_columnCount(QTableWidgetSelectionRangeH handle);
C_EXPORT QTableWidgetItemH QTableWidgetItem_Create(int type);
C_EXPORT void QTableWidgetItem_Destroy(QTableWidgetItemH handle);
C_EXPORT QTableWidgetItemH QTableWidgetItem_Create2(PWideString text, int type);
C_EXPORT QTableWidgetItemH QTableWidgetItem_Create3(const QIconH icon, PWideString text, int type);
C_EXPORT QTableWidgetItemH QTableWidgetItem_Create4(const QTableWidgetItemH other);
C_EXPORT QTableWidgetItemH QTableWidgetItem_clone(QTableWidgetItemH handle);
C_EXPORT QTableWidgetH QTableWidgetItem_tableWidget(QTableWidgetItemH handle);
C_EXPORT int QTableWidgetItem_row(QTableWidgetItemH handle);
C_EXPORT int QTableWidgetItem_column(QTableWidgetItemH handle);
C_EXPORT void QTableWidgetItem_setSelected(QTableWidgetItemH handle, bool select);
C_EXPORT bool QTableWidgetItem_isSelected(QTableWidgetItemH handle);
C_EXPORT unsigned int QTableWidgetItem_flags(QTableWidgetItemH handle);
C_EXPORT void QTableWidgetItem_setFlags(QTableWidgetItemH handle, unsigned int flags);
C_EXPORT void QTableWidgetItem_text(QTableWidgetItemH handle, PWideString retval);
C_EXPORT void QTableWidgetItem_setText(QTableWidgetItemH handle, PWideString text);
C_EXPORT void QTableWidgetItem_icon(QTableWidgetItemH handle, QIconH retval);
C_EXPORT void QTableWidgetItem_setIcon(QTableWidgetItemH handle, const QIconH icon);
C_EXPORT void QTableWidgetItem_statusTip(QTableWidgetItemH handle, PWideString retval);
C_EXPORT void QTableWidgetItem_setStatusTip(QTableWidgetItemH handle, PWideString statusTip);
C_EXPORT void QTableWidgetItem_toolTip(QTableWidgetItemH handle, PWideString retval);
C_EXPORT void QTableWidgetItem_setToolTip(QTableWidgetItemH handle, PWideString toolTip);
C_EXPORT void QTableWidgetItem_whatsThis(QTableWidgetItemH handle, PWideString retval);
C_EXPORT void QTableWidgetItem_setWhatsThis(QTableWidgetItemH handle, PWideString whatsThis);
C_EXPORT void QTableWidgetItem_font(QTableWidgetItemH handle, QFontH retval);
C_EXPORT void QTableWidgetItem_setFont(QTableWidgetItemH handle, const QFontH font);
C_EXPORT int QTableWidgetItem_textAlignment(QTableWidgetItemH handle);
C_EXPORT void QTableWidgetItem_setTextAlignment(QTableWidgetItemH handle, int alignment);
C_EXPORT void QTableWidgetItem_background(QTableWidgetItemH handle, QBrushH retval);
C_EXPORT void QTableWidgetItem_setBackground(QTableWidgetItemH handle, const QBrushH brush);
C_EXPORT void QTableWidgetItem_foreground(QTableWidgetItemH handle, QBrushH retval);
C_EXPORT void QTableWidgetItem_setForeground(QTableWidgetItemH handle, const QBrushH brush);
C_EXPORT Qt::CheckState QTableWidgetItem_checkState(QTableWidgetItemH handle);
C_EXPORT void QTableWidgetItem_setCheckState(QTableWidgetItemH handle, Qt::CheckState state);
C_EXPORT void QTableWidgetItem_sizeHint(QTableWidgetItemH handle, PSize retval);
C_EXPORT void QTableWidgetItem_setSizeHint(QTableWidgetItemH handle, const QSizeH size);
C_EXPORT void QTableWidgetItem_data(QTableWidgetItemH handle, QVariantH retval, int role);
C_EXPORT void QTableWidgetItem_setData(QTableWidgetItemH handle, int role, const QVariantH value);
C_EXPORT void QTableWidgetItem_read(QTableWidgetItemH handle, QDataStreamH in);
C_EXPORT void QTableWidgetItem_write(QTableWidgetItemH handle, QDataStreamH out);
C_EXPORT int QTableWidgetItem_type(QTableWidgetItemH handle);
C_EXPORT QTableWidgetH QTableWidget_Create(QWidgetH parent);
C_EXPORT void QTableWidget_Destroy(QTableWidgetH handle);
C_EXPORT QTableWidgetH QTableWidget_Create2(int rows, int columns, QWidgetH parent);
C_EXPORT void QTableWidget_setRowCount(QTableWidgetH handle, int rows);
C_EXPORT int QTableWidget_rowCount(QTableWidgetH handle);
C_EXPORT void QTableWidget_setColumnCount(QTableWidgetH handle, int columns);
C_EXPORT int QTableWidget_columnCount(QTableWidgetH handle);
C_EXPORT int QTableWidget_row(QTableWidgetH handle, const QTableWidgetItemH item);
C_EXPORT int QTableWidget_column(QTableWidgetH handle, const QTableWidgetItemH item);
C_EXPORT QTableWidgetItemH QTableWidget_item(QTableWidgetH handle, int row, int column);
C_EXPORT void QTableWidget_setItem(QTableWidgetH handle, int row, int column, QTableWidgetItemH item);
C_EXPORT QTableWidgetItemH QTableWidget_takeItem(QTableWidgetH handle, int row, int column);
C_EXPORT QTableWidgetItemH QTableWidget_verticalHeaderItem(QTableWidgetH handle, int row);
C_EXPORT void QTableWidget_setVerticalHeaderItem(QTableWidgetH handle, int row, QTableWidgetItemH item);
C_EXPORT QTableWidgetItemH QTableWidget_takeVerticalHeaderItem(QTableWidgetH handle, int row);
C_EXPORT QTableWidgetItemH QTableWidget_horizontalHeaderItem(QTableWidgetH handle, int column);
C_EXPORT void QTableWidget_setHorizontalHeaderItem(QTableWidgetH handle, int column, QTableWidgetItemH item);
C_EXPORT QTableWidgetItemH QTableWidget_takeHorizontalHeaderItem(QTableWidgetH handle, int column);
C_EXPORT void QTableWidget_setVerticalHeaderLabels(QTableWidgetH handle, const QStringListH labels);
C_EXPORT void QTableWidget_setHorizontalHeaderLabels(QTableWidgetH handle, const QStringListH labels);
C_EXPORT int QTableWidget_currentRow(QTableWidgetH handle);
C_EXPORT int QTableWidget_currentColumn(QTableWidgetH handle);
C_EXPORT QTableWidgetItemH QTableWidget_currentItem(QTableWidgetH handle);
C_EXPORT void QTableWidget_setCurrentItem(QTableWidgetH handle, QTableWidgetItemH item);
C_EXPORT void QTableWidget_setCurrentItem2(QTableWidgetH handle, QTableWidgetItemH item, unsigned int command);
C_EXPORT void QTableWidget_setCurrentCell(QTableWidgetH handle, int row, int column);
C_EXPORT void QTableWidget_setCurrentCell2(QTableWidgetH handle, int row, int column, unsigned int command);
C_EXPORT void QTableWidget_sortItems(QTableWidgetH handle, int column, Qt::SortOrder order);
C_EXPORT void QTableWidget_setSortingEnabled(QTableWidgetH handle, bool enable);
C_EXPORT bool QTableWidget_isSortingEnabled(QTableWidgetH handle);
C_EXPORT void QTableWidget_editItem(QTableWidgetH handle, QTableWidgetItemH item);
C_EXPORT void QTableWidget_openPersistentEditor(QTableWidgetH handle, QTableWidgetItemH item);
C_EXPORT void QTableWidget_closePersistentEditor(QTableWidgetH handle, QTableWidgetItemH item);
C_EXPORT QWidgetH QTableWidget_cellWidget(QTableWidgetH handle, int row, int column);
C_EXPORT void QTableWidget_setCellWidget(QTableWidgetH handle, int row, int column, QWidgetH widget);
C_EXPORT void QTableWidget_removeCellWidget(QTableWidgetH handle, int row, int column);
C_EXPORT void QTableWidget_setRangeSelected(QTableWidgetH handle, const QTableWidgetSelectionRangeH range, bool select);
C_EXPORT void QTableWidget_selectedItems(QTableWidgetH handle, PPtrIntArray retval);
C_EXPORT void QTableWidget_findItems(QTableWidgetH handle, PPtrIntArray retval, PWideString text, unsigned int flags);
C_EXPORT int QTableWidget_visualRow(QTableWidgetH handle, int logicalRow);
C_EXPORT int QTableWidget_visualColumn(QTableWidgetH handle, int logicalColumn);
C_EXPORT QTableWidgetItemH QTableWidget_itemAt(QTableWidgetH handle, const QPointH p);
C_EXPORT QTableWidgetItemH QTableWidget_itemAt2(QTableWidgetH handle, int x, int y);
C_EXPORT void QTableWidget_visualItemRect(QTableWidgetH handle, PRect retval, const QTableWidgetItemH item);
C_EXPORT const QTableWidgetItemH QTableWidget_itemPrototype(QTableWidgetH handle);
C_EXPORT void QTableWidget_setItemPrototype(QTableWidgetH handle, const QTableWidgetItemH item);
C_EXPORT void QTableWidget_scrollToItem(QTableWidgetH handle, const QTableWidgetItemH item, QAbstractItemView::ScrollHint hint);
C_EXPORT void QTableWidget_insertRow(QTableWidgetH handle, int row);
C_EXPORT void QTableWidget_insertColumn(QTableWidgetH handle, int column);
C_EXPORT void QTableWidget_removeRow(QTableWidgetH handle, int row);
C_EXPORT void QTableWidget_removeColumn(QTableWidgetH handle, int column);
C_EXPORT void QTableWidget_clear(QTableWidgetH handle);
C_EXPORT void QTableWidget_clearContents(QTableWidgetH handle);

#endif
