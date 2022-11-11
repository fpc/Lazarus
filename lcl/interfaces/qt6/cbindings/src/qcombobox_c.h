//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOMBOBOX_C_H
#define QCOMBOBOX_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QComboBoxH QComboBox_Create(QWidgetH parent);
C_EXPORT void QComboBox_Destroy(QComboBoxH handle);
C_EXPORT int QComboBox_maxVisibleItems(QComboBoxH handle);
C_EXPORT void QComboBox_setMaxVisibleItems(QComboBoxH handle, int maxItems);
C_EXPORT int QComboBox_count(QComboBoxH handle);
C_EXPORT void QComboBox_setMaxCount(QComboBoxH handle, int max);
C_EXPORT int QComboBox_maxCount(QComboBoxH handle);
C_EXPORT bool QComboBox_duplicatesEnabled(QComboBoxH handle);
C_EXPORT void QComboBox_setDuplicatesEnabled(QComboBoxH handle, bool enable);
C_EXPORT void QComboBox_setFrame(QComboBoxH handle, bool AnonParam1);
C_EXPORT bool QComboBox_hasFrame(QComboBoxH handle);
C_EXPORT int QComboBox_findText(QComboBoxH handle, PWideString text, unsigned int flags);
C_EXPORT int QComboBox_findData(QComboBoxH handle, const QVariantH data, Qt::ItemDataRole role, unsigned int flags);
C_EXPORT QComboBox::InsertPolicy QComboBox_insertPolicy(QComboBoxH handle);
C_EXPORT void QComboBox_setInsertPolicy(QComboBoxH handle, QComboBox::InsertPolicy policy);
C_EXPORT QComboBox::SizeAdjustPolicy QComboBox_sizeAdjustPolicy(QComboBoxH handle);
C_EXPORT void QComboBox_setSizeAdjustPolicy(QComboBoxH handle, QComboBox::SizeAdjustPolicy policy);
C_EXPORT int QComboBox_minimumContentsLength(QComboBoxH handle);
C_EXPORT void QComboBox_setMinimumContentsLength(QComboBoxH handle, int characters);
C_EXPORT void QComboBox_iconSize(QComboBoxH handle, PSize retval);
C_EXPORT void QComboBox_setIconSize(QComboBoxH handle, const QSizeH size);
C_EXPORT bool QComboBox_isEditable(QComboBoxH handle);
C_EXPORT void QComboBox_setEditable(QComboBoxH handle, bool editable);
C_EXPORT void QComboBox_setLineEdit(QComboBoxH handle, QLineEditH edit);
C_EXPORT QLineEditH QComboBox_lineEdit(QComboBoxH handle);
C_EXPORT void QComboBox_setValidator(QComboBoxH handle, const QValidatorH v);
C_EXPORT const QValidatorH QComboBox_validator(QComboBoxH handle);
C_EXPORT void QComboBox_setCompleter(QComboBoxH handle, QCompleterH c);
C_EXPORT QCompleterH QComboBox_completer(QComboBoxH handle);
C_EXPORT QAbstractItemDelegateH QComboBox_itemDelegate(QComboBoxH handle);
C_EXPORT void QComboBox_setItemDelegate(QComboBoxH handle, QAbstractItemDelegateH delegate);
C_EXPORT QAbstractItemModelH QComboBox_model(QComboBoxH handle);
C_EXPORT void QComboBox_setModel(QComboBoxH handle, QAbstractItemModelH model);
C_EXPORT void QComboBox_rootModelIndex(QComboBoxH handle, QModelIndexH retval);
C_EXPORT void QComboBox_setRootModelIndex(QComboBoxH handle, const QModelIndexH index);
C_EXPORT int QComboBox_modelColumn(QComboBoxH handle);
C_EXPORT void QComboBox_setModelColumn(QComboBoxH handle, int visibleColumn);
C_EXPORT int QComboBox_currentIndex(QComboBoxH handle);
C_EXPORT void QComboBox_currentText(QComboBoxH handle, PWideString retval);
C_EXPORT void QComboBox_itemText(QComboBoxH handle, PWideString retval, int index);
C_EXPORT void QComboBox_itemIcon(QComboBoxH handle, QIconH retval, int index);
C_EXPORT void QComboBox_itemData(QComboBoxH handle, QVariantH retval, int index, Qt::ItemDataRole role);
C_EXPORT void QComboBox_addItem(QComboBoxH handle, PWideString text, const QVariantH userData);
C_EXPORT void QComboBox_addItem2(QComboBoxH handle, const QIconH icon, PWideString text, const QVariantH userData);
C_EXPORT void QComboBox_addItems(QComboBoxH handle, const QStringListH texts);
C_EXPORT void QComboBox_insertItem(QComboBoxH handle, int index, PWideString text, const QVariantH userData);
C_EXPORT void QComboBox_insertItem2(QComboBoxH handle, int index, const QIconH icon, PWideString text, const QVariantH userData);
C_EXPORT void QComboBox_insertItems(QComboBoxH handle, int index, const QStringListH texts);
C_EXPORT void QComboBox_insertSeparator(QComboBoxH handle, int index);
C_EXPORT void QComboBox_removeItem(QComboBoxH handle, int index);
C_EXPORT void QComboBox_setItemText(QComboBoxH handle, int index, PWideString text);
C_EXPORT void QComboBox_setItemIcon(QComboBoxH handle, int index, const QIconH icon);
C_EXPORT void QComboBox_setItemData(QComboBoxH handle, int index, const QVariantH value, Qt::ItemDataRole role);
C_EXPORT QAbstractItemViewH QComboBox_view(QComboBoxH handle);
C_EXPORT void QComboBox_setView(QComboBoxH handle, QAbstractItemViewH itemView);
C_EXPORT void QComboBox_sizeHint(QComboBoxH handle, PSize retval);
C_EXPORT void QComboBox_minimumSizeHint(QComboBoxH handle, PSize retval);
C_EXPORT void QComboBox_showPopup(QComboBoxH handle);
C_EXPORT void QComboBox_hidePopup(QComboBoxH handle);
C_EXPORT bool QComboBox_event(QComboBoxH handle, QEventH event);
C_EXPORT void QComboBox_inputMethodQuery(QComboBoxH handle, QVariantH retval, Qt::InputMethodQuery AnonParam1);
C_EXPORT void QComboBox_clear(QComboBoxH handle);
C_EXPORT void QComboBox_clearEditText(QComboBoxH handle);
C_EXPORT void QComboBox_setEditText(QComboBoxH handle, PWideString text);
C_EXPORT void QComboBox_setCurrentIndex(QComboBoxH handle, int index);
C_EXPORT void QComboBox_setCurrentText(QComboBoxH handle, PWideString text);
C_EXPORT void QComboBox_placeholderText(QComboBoxH handle, PWideString retval);
C_EXPORT void QComboBox_setPlaceholderText(QComboBoxH handle, PWideString text);

#endif
