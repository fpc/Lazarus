//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcombobox_c.h"

QComboBoxH QComboBox_Create(QWidgetH parent)
{
	return (QComboBoxH) new QComboBox((QWidget*)parent);
}

void QComboBox_Destroy(QComboBoxH handle)
{
	delete (QComboBox *)handle;
}

int QComboBox_maxVisibleItems(QComboBoxH handle)
{
	return (int) ((QComboBox *)handle)->maxVisibleItems();
}

void QComboBox_setMaxVisibleItems(QComboBoxH handle, int maxItems)
{
	((QComboBox *)handle)->setMaxVisibleItems(maxItems);
}

int QComboBox_count(QComboBoxH handle)
{
	return (int) ((QComboBox *)handle)->count();
}

void QComboBox_setMaxCount(QComboBoxH handle, int max)
{
	((QComboBox *)handle)->setMaxCount(max);
}

int QComboBox_maxCount(QComboBoxH handle)
{
	return (int) ((QComboBox *)handle)->maxCount();
}

bool QComboBox_duplicatesEnabled(QComboBoxH handle)
{
	return (bool) ((QComboBox *)handle)->duplicatesEnabled();
}

void QComboBox_setDuplicatesEnabled(QComboBoxH handle, bool enable)
{
	((QComboBox *)handle)->setDuplicatesEnabled(enable);
}

void QComboBox_setFrame(QComboBoxH handle, bool AnonParam1)
{
	((QComboBox *)handle)->setFrame(AnonParam1);
}

bool QComboBox_hasFrame(QComboBoxH handle)
{
	return (bool) ((QComboBox *)handle)->hasFrame();
}

int QComboBox_findText(QComboBoxH handle, PWideString text, unsigned int flags)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (int) ((QComboBox *)handle)->findText(t_text, (Qt::MatchFlags)flags);
}

int QComboBox_findData(QComboBoxH handle, const QVariantH data, Qt::ItemDataRole role, unsigned int flags)
{
	return (int) ((QComboBox *)handle)->findData(*(const QVariant*)data, role, (Qt::MatchFlags)flags);
}

QComboBox::InsertPolicy QComboBox_insertPolicy(QComboBoxH handle)
{
	return (QComboBox::InsertPolicy) ((QComboBox *)handle)->insertPolicy();
}

void QComboBox_setInsertPolicy(QComboBoxH handle, QComboBox::InsertPolicy policy)
{
	((QComboBox *)handle)->setInsertPolicy(policy);
}

QComboBox::SizeAdjustPolicy QComboBox_sizeAdjustPolicy(QComboBoxH handle)
{
	return (QComboBox::SizeAdjustPolicy) ((QComboBox *)handle)->sizeAdjustPolicy();
}

void QComboBox_setSizeAdjustPolicy(QComboBoxH handle, QComboBox::SizeAdjustPolicy policy)
{
	((QComboBox *)handle)->setSizeAdjustPolicy(policy);
}

int QComboBox_minimumContentsLength(QComboBoxH handle)
{
	return (int) ((QComboBox *)handle)->minimumContentsLength();
}

void QComboBox_setMinimumContentsLength(QComboBoxH handle, int characters)
{
	((QComboBox *)handle)->setMinimumContentsLength(characters);
}

void QComboBox_iconSize(QComboBoxH handle, PSize retval)
{
	*(QSize *)retval = ((QComboBox *)handle)->iconSize();
}

void QComboBox_setIconSize(QComboBoxH handle, const QSizeH size)
{
	((QComboBox *)handle)->setIconSize(*(const QSize*)size);
}

bool QComboBox_isEditable(QComboBoxH handle)
{
	return (bool) ((QComboBox *)handle)->isEditable();
}

void QComboBox_setEditable(QComboBoxH handle, bool editable)
{
	((QComboBox *)handle)->setEditable(editable);
}

void QComboBox_setLineEdit(QComboBoxH handle, QLineEditH edit)
{
	((QComboBox *)handle)->setLineEdit((QLineEdit*)edit);
}

QLineEditH QComboBox_lineEdit(QComboBoxH handle)
{
	return (QLineEditH) ((QComboBox *)handle)->lineEdit();
}

void QComboBox_setValidator(QComboBoxH handle, const QValidatorH v)
{
	((QComboBox *)handle)->setValidator((const QValidator*)v);
}

const QValidatorH QComboBox_validator(QComboBoxH handle)
{
	return (const QValidatorH) ((QComboBox *)handle)->validator();
}

void QComboBox_setCompleter(QComboBoxH handle, QCompleterH c)
{
	((QComboBox *)handle)->setCompleter((QCompleter*)c);
}

QCompleterH QComboBox_completer(QComboBoxH handle)
{
	return (QCompleterH) ((QComboBox *)handle)->completer();
}

QAbstractItemDelegateH QComboBox_itemDelegate(QComboBoxH handle)
{
	return (QAbstractItemDelegateH) ((QComboBox *)handle)->itemDelegate();
}

void QComboBox_setItemDelegate(QComboBoxH handle, QAbstractItemDelegateH delegate)
{
	((QComboBox *)handle)->setItemDelegate((QAbstractItemDelegate*)delegate);
}

QAbstractItemModelH QComboBox_model(QComboBoxH handle)
{
	return (QAbstractItemModelH) ((QComboBox *)handle)->model();
}

void QComboBox_setModel(QComboBoxH handle, QAbstractItemModelH model)
{
	((QComboBox *)handle)->setModel((QAbstractItemModel*)model);
}

void QComboBox_rootModelIndex(QComboBoxH handle, QModelIndexH retval)
{
	*(QModelIndex *)retval = ((QComboBox *)handle)->rootModelIndex();
}

void QComboBox_setRootModelIndex(QComboBoxH handle, const QModelIndexH index)
{
	((QComboBox *)handle)->setRootModelIndex(*(const QModelIndex*)index);
}

int QComboBox_modelColumn(QComboBoxH handle)
{
	return (int) ((QComboBox *)handle)->modelColumn();
}

void QComboBox_setModelColumn(QComboBoxH handle, int visibleColumn)
{
	((QComboBox *)handle)->setModelColumn(visibleColumn);
}

int QComboBox_currentIndex(QComboBoxH handle)
{
	return (int) ((QComboBox *)handle)->currentIndex();
}

void QComboBox_currentText(QComboBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QComboBox *)handle)->currentText();
	copyQStringToPWideString(t_retval, retval);
}

void QComboBox_itemText(QComboBoxH handle, PWideString retval, int index)
{
	QString t_retval;
	t_retval = ((QComboBox *)handle)->itemText(index);
	copyQStringToPWideString(t_retval, retval);
}

void QComboBox_itemIcon(QComboBoxH handle, QIconH retval, int index)
{
	*(QIcon *)retval = ((QComboBox *)handle)->itemIcon(index);
}

void QComboBox_itemData(QComboBoxH handle, QVariantH retval, int index, Qt::ItemDataRole role)
{
	*(QVariant *)retval = ((QComboBox *)handle)->itemData(index, role);
}

void QComboBox_addItem(QComboBoxH handle, PWideString text, const QVariantH userData)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QComboBox *)handle)->addItem(t_text, *(const QVariant*)userData);
}

void QComboBox_addItem2(QComboBoxH handle, const QIconH icon, PWideString text, const QVariantH userData)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QComboBox *)handle)->addItem(*(const QIcon*)icon, t_text, *(const QVariant*)userData);
}

void QComboBox_addItems(QComboBoxH handle, const QStringListH texts)
{
	((QComboBox *)handle)->addItems(*(const QStringList*)texts);
}

void QComboBox_insertItem(QComboBoxH handle, int index, PWideString text, const QVariantH userData)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QComboBox *)handle)->insertItem(index, t_text, *(const QVariant*)userData);
}

void QComboBox_insertItem2(QComboBoxH handle, int index, const QIconH icon, PWideString text, const QVariantH userData)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QComboBox *)handle)->insertItem(index, *(const QIcon*)icon, t_text, *(const QVariant*)userData);
}

void QComboBox_insertItems(QComboBoxH handle, int index, const QStringListH texts)
{
	((QComboBox *)handle)->insertItems(index, *(const QStringList*)texts);
}

void QComboBox_insertSeparator(QComboBoxH handle, int index)
{
	((QComboBox *)handle)->insertSeparator(index);
}

void QComboBox_removeItem(QComboBoxH handle, int index)
{
	((QComboBox *)handle)->removeItem(index);
}

void QComboBox_setItemText(QComboBoxH handle, int index, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QComboBox *)handle)->setItemText(index, t_text);
}

void QComboBox_setItemIcon(QComboBoxH handle, int index, const QIconH icon)
{
	((QComboBox *)handle)->setItemIcon(index, *(const QIcon*)icon);
}

void QComboBox_setItemData(QComboBoxH handle, int index, const QVariantH value, Qt::ItemDataRole role)
{
	((QComboBox *)handle)->setItemData(index, *(const QVariant*)value, role);
}

QAbstractItemViewH QComboBox_view(QComboBoxH handle)
{
	return (QAbstractItemViewH) ((QComboBox *)handle)->view();
}

void QComboBox_setView(QComboBoxH handle, QAbstractItemViewH itemView)
{
	((QComboBox *)handle)->setView((QAbstractItemView*)itemView);
}

void QComboBox_sizeHint(QComboBoxH handle, PSize retval)
{
	*(QSize *)retval = ((QComboBox *)handle)->sizeHint();
}

void QComboBox_minimumSizeHint(QComboBoxH handle, PSize retval)
{
	*(QSize *)retval = ((QComboBox *)handle)->minimumSizeHint();
}

void QComboBox_showPopup(QComboBoxH handle)
{
	((QComboBox *)handle)->showPopup();
}

void QComboBox_hidePopup(QComboBoxH handle)
{
	((QComboBox *)handle)->hidePopup();
}

bool QComboBox_event(QComboBoxH handle, QEventH event)
{
	return (bool) ((QComboBox *)handle)->event((QEvent*)event);
}

void QComboBox_inputMethodQuery(QComboBoxH handle, QVariantH retval, Qt::InputMethodQuery AnonParam1)
{
	*(QVariant *)retval = ((QComboBox *)handle)->inputMethodQuery(AnonParam1);
}

void QComboBox_clear(QComboBoxH handle)
{
	((QComboBox *)handle)->clear();
}

void QComboBox_clearEditText(QComboBoxH handle)
{
	((QComboBox *)handle)->clearEditText();
}

void QComboBox_setEditText(QComboBoxH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QComboBox *)handle)->setEditText(t_text);
}

void QComboBox_setCurrentIndex(QComboBoxH handle, int index)
{
	((QComboBox *)handle)->setCurrentIndex(index);
}

void QComboBox_setCurrentText(QComboBoxH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QComboBox *)handle)->setCurrentText(t_text);
}

void QComboBox_placeholderText(QComboBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QComboBox *)handle)->placeholderText();
	copyQStringToPWideString(t_retval, retval);
}

void QComboBox_setPlaceholderText(QComboBoxH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QComboBox *)handle)->setPlaceholderText(t_text);
}

