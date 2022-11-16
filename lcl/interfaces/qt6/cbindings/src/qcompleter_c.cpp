//******************************************************************************
//  Copyright (c) 2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcompleter_c.h"

QCompleterH QCompleter_Create()
{
  return (QCompleterH) new QCompleter();
}

QCompleterH QCompleter_Create2(QObjectH parent)
{
  return (QCompleterH) new QCompleter((QObject*)parent);
}

QCompleterH QCompleter_Create3(QAbstractItemModelH model, QObjectH parent)
{
  return (QCompleterH) new QCompleter((QAbstractItemModel *) model, (QObject *) parent);
}

QCompleterH QCompleter_Create4(QStringListH list, QObjectH parent)
{
  return (QCompleterH) new QCompleter(*(const QStringList *) list, (QObject *)parent);
}

void QCompleter_Destroy(QCompleterH handle)
{
  delete (QCompleter *)handle;
}

Qt::CaseSensitivity QCompleter_caseSensitivity(QCompleterH handle)
{
  return (Qt::CaseSensitivity) ((QCompleter *)handle)->caseSensitivity();
}

int QCompleter_completionColumn(QCompleterH handle)
{
  return (int) ((QCompleter *)handle)->completionColumn();
}

int QCompleter_completionCount(QCompleterH handle)
{
  return (int) ((QCompleter *)handle)->completionCount();
}

QCompleter::CompletionMode QCompleter_completionMode(QCompleterH handle)
{
  return (QCompleter::CompletionMode) ((QCompleter *)handle)->completionMode();
}

QAbstractItemModelH QCompleter_completionModel(QCompleterH handle)
{
  return (QAbstractItemModelH) ((QCompleter *)handle)->completionModel();
}

void QCompleter_completionPrefix(QCompleterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QCompleter *)handle)->completionPrefix();
	copyQStringToPWideString(t_retval, retval);
}

int QCompleter_completionRole(QCompleterH handle)
{
  return (int) ((QCompleter *)handle)->completionRole();
}

void QCompleter_currentCompletion(QCompleterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QCompleter *)handle)->currentCompletion();
	copyQStringToPWideString(t_retval, retval);
}

void QCompleter_currentIndex(QCompleterH handle, QModelIndexH retval)
{
  *(QModelIndex*)retval = ((QCompleter *)handle)->currentIndex();
}

int QCompleter_currentRow(QCompleterH handle)
{
  return (int) ((QCompleter *)handle)->currentRow();
}

Qt::MatchFlags QCompleter_filterMode(QCompleterH handle)
{
  return (Qt::MatchFlags) ((QCompleter *)handle)->filterMode();
}

int QCompleter_maxVisibleItems(QCompleterH handle)
{
  return (int) ((QCompleter *)handle)->maxVisibleItems();
}

QAbstractItemModelH QCompleter_model(QCompleterH handle)
{
  return (QAbstractItemModelH) ((QCompleter *)handle)->model();
}

QCompleter::ModelSorting QCompleter_modelSorting(QCompleterH handle)
{
  return (QCompleter::ModelSorting) ((QCompleter *)handle)->modelSorting();
}

void QCompleter_pathFromIndex(QCompleterH handle, QModelIndexH model, PWideString retval)
{
	QString t_retval;
	t_retval = ((QCompleter *)handle)->pathFromIndex(*(QModelIndex *)model);
	copyQStringToPWideString(t_retval, retval);
}

QAbstractItemViewH QCompleter_popup(QCompleterH handle)
{
  return (QAbstractItemViewH) ((QCompleter *)handle)->popup();
}

void QCompleter_setCaseSensitivity(QCompleterH handle, Qt::CaseSensitivity caseSensitivity)
{
  ((QCompleter *)handle)->setCaseSensitivity(caseSensitivity);
}

void QCompleter_setCompletionColumn(QCompleterH handle, int column)
{
  ((QCompleter *)handle)->setCompletionColumn(column);
}

void QCompleter_setCompletionMode(QCompleterH handle, QCompleter::CompletionMode mode)
{
  ((QCompleter *)handle)->setCompletionMode(mode);
}

void QCompleter_setCompletionRole(QCompleterH handle, int role)
{
  ((QCompleter *)handle)->setCompletionRole(role);
}

void QCompleter_setCurrentRow(QCompleterH handle, int row)
{
  ((QCompleter *)handle)->setCurrentRow(row);
}

void QCompleter_setFilterMode(QCompleterH handle, Qt::MatchFlags filterMode)
{
  ((QCompleter *)handle)->setFilterMode(filterMode);
}

void QCompleter_setMaxVisibleItems(QCompleterH handle, int maxItems)
{
  ((QCompleter *)handle)->setMaxVisibleItems(maxItems);
}

void QCompleter_setModel(QCompleterH handle, QAbstractItemModelH model)
{
  ((QCompleter *)handle)->setModel((QAbstractItemModel*)model);
}

void QCompleter_setModelSorting(QCompleterH handle, QCompleter::ModelSorting sorting)
{
  ((QCompleter *)handle)->setModelSorting(sorting);
}

void QCompleter_setPopup(QCompleterH handle, QAbstractItemViewH popup)
{
  ((QCompleter *)handle)->setPopup((QAbstractItemView *)popup);
}

void QCompleter_setWidget(QCompleterH handle, QWidgetH widget)
{
  ((QCompleter *)handle)->setWidget((QWidget *)widget);
}

void QCompleter_splitPath(QCompleterH handle, PWideString path, QStringListH retval)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	*(QStringList *)retval = ((QCompleter *)handle)->splitPath(t_path);
}

QWidgetH QCompleter_widget(QCompleterH handle)
{
  return (QWidgetH) ((QCompleter *)handle)->widget();
}

bool QCompleter_wrapAround(QCompleterH handle)
{
  return (bool) ((QCompleter *)handle)->wrapAround();
}

void QCompleter_complete(QCompleterH handle, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
  ((QCompleter *)handle)->complete(t_rect);
}

void QCompleter_setCompletionPrefix(QCompleterH handle, PWideString prefix)
{
	QString t_prefix;
	copyPWideStringToQString(prefix, t_prefix);
	((QCompleter *)handle)->setCompletionPrefix(t_prefix);
}

void QCompleter_setWrapAround(QCompleterH handle, bool wrap)
{
  ((QCompleter *)handle)->setWrapAround(wrap);
}



