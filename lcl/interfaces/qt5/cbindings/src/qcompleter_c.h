//******************************************************************************
//  Copyright (c) 2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOMPLETER_C_H
#define QCOMPLETER_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QCompleterH QCompleter_Create();
C_EXPORT QCompleterH QCompleter_Create2(QObjectH parent);
C_EXPORT QCompleterH QCompleter_Create3(QAbstractItemModelH model, QObjectH parent);
C_EXPORT QCompleterH QCompleter_Create4(QStringListH list, QObjectH parent);
C_EXPORT void QCompleter_Destroy(QCompleterH handle);
C_EXPORT Qt::CaseSensitivity QCompleter_caseSensitivity(QCompleterH handle);
C_EXPORT int QCompleter_completionColumn(QCompleterH handle);
C_EXPORT int QCompleter_completionCount(QCompleterH handle);
C_EXPORT QCompleter::CompletionMode QCompleter_completionMode(QCompleterH handle);
C_EXPORT QAbstractItemModelH QCompleter_completionModel(QCompleterH handle);
C_EXPORT void QCompleter_completionPrefix(QCompleterH handle, PWideString retval);
C_EXPORT int QCompleter_completionRole(QCompleterH handle);
C_EXPORT void QCompleter_currentCompletion(QCompleterH handle, PWideString retval);
C_EXPORT void QCompleter_currentIndex(QCompleterH handle,QModelIndexH retval);
C_EXPORT int QCompleter_currentRow(QCompleterH handle);
C_EXPORT Qt::MatchFlags QCompleter_filterMode(QCompleterH handle);
C_EXPORT int QCompleter_maxVisibleItems(QCompleterH handle);
C_EXPORT QAbstractItemModelH QCompleter_model(QCompleterH handle);
C_EXPORT QCompleter::ModelSorting QCompleter_modelSorting(QCompleterH handle);
C_EXPORT void QCompleter_pathFromIndex(QCompleterH handle, QModelIndexH model, PWideString retval);
C_EXPORT QAbstractItemViewH QCompleter_popup(QCompleterH handle);
C_EXPORT void QCompleter_setCaseSensitivity(QCompleterH handle, Qt::CaseSensitivity caseSensitivity);
C_EXPORT void QCompleter_setCompletionColumn(QCompleterH handle, int column);
C_EXPORT void QCompleter_setCompletionMode(QCompleterH handle, QCompleter::CompletionMode mode);
C_EXPORT void QCompleter_setCompletionRole(QCompleterH handle, int role);
C_EXPORT void QCompleter_setCurrentRow(QCompleterH handle, int row);
C_EXPORT void QCompleter_setFilterMode(QCompleterH handle, Qt::MatchFlags filterMode);
C_EXPORT void QCompleter_setMaxVisibleItems(QCompleterH handle, int maxItems);
C_EXPORT void QCompleter_setModel(QCompleterH handle, QAbstractItemModelH model);
C_EXPORT void QCompleter_setModelSorting(QCompleterH handle, QCompleter::ModelSorting sorting);
C_EXPORT void QCompleter_setPopup(QCompleterH handle, QAbstractItemViewH popup);
C_EXPORT void QCompleter_setWidget(QCompleterH handle, QWidgetH widget);
C_EXPORT void QCompleter_splitPath(QCompleterH handle, PWideString path, QStringListH retval);
C_EXPORT QWidgetH QCompleter_widget(QCompleterH handle);
C_EXPORT bool QCompleter_wrapAround(QCompleterH handle);
C_EXPORT void QCompleter_complete(QCompleterH handle, PRect rect);
C_EXPORT void QCompleter_setCompletionPrefix(QCompleterH handle, PWideString prefix);
C_EXPORT void QCompleter_setWrapAround(QCompleterH handle, bool wrap);

#endif
