//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFILEDIALOG_C_H
#define QFILEDIALOG_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QFileDialogH QFileDialog_Create(QWidgetH parent, unsigned int f);
C_EXPORT void QFileDialog_Destroy(QFileDialogH handle);
C_EXPORT QFileDialogH QFileDialog_Create2(QWidgetH parent, PWideString caption, PWideString directory, PWideString filter);
C_EXPORT void QFileDialog_setDirectory(QFileDialogH handle, PWideString directory);
C_EXPORT void QFileDialog_setDirectory2(QFileDialogH handle, const QDirH directory);
C_EXPORT void QFileDialog_directory(QFileDialogH handle, QDirH retval);
C_EXPORT void QFileDialog_selectFile(QFileDialogH handle, PWideString filename);
C_EXPORT void QFileDialog_selectedFiles(QFileDialogH handle, QStringListH retval);
C_EXPORT void QFileDialog_setNameFilter(QFileDialogH handle, PWideString filter);
C_EXPORT void QFileDialog_setNameFilters(QFileDialogH handle, const QStringListH filters);
C_EXPORT void QFileDialog_nameFilters(QFileDialogH handle, QStringListH retval);
C_EXPORT void QFileDialog_selectNameFilter(QFileDialogH handle, PWideString filter);
C_EXPORT void QFileDialog_selectedNameFilter(QFileDialogH handle, PWideString retval);
C_EXPORT unsigned int QFileDialog_filter(QFileDialogH handle);
C_EXPORT void QFileDialog_setFilter(QFileDialogH handle, unsigned int filters);
C_EXPORT void QFileDialog_setViewMode(QFileDialogH handle, QFileDialog::ViewMode mode);
C_EXPORT QFileDialog::ViewMode QFileDialog_viewMode(QFileDialogH handle);
C_EXPORT void QFileDialog_setFileMode(QFileDialogH handle, QFileDialog::FileMode mode);
C_EXPORT QFileDialog::FileMode QFileDialog_fileMode(QFileDialogH handle);
C_EXPORT void QFileDialog_setAcceptMode(QFileDialogH handle, QFileDialog::AcceptMode mode);
C_EXPORT QFileDialog::AcceptMode QFileDialog_acceptMode(QFileDialogH handle);
C_EXPORT void QFileDialog_saveState(QFileDialogH handle, QByteArrayH retval);
C_EXPORT bool QFileDialog_restoreState(QFileDialogH handle, const QByteArrayH state);
C_EXPORT void QFileDialog_setDefaultSuffix(QFileDialogH handle, PWideString suffix);
C_EXPORT void QFileDialog_defaultSuffix(QFileDialogH handle, PWideString retval);
C_EXPORT void QFileDialog_setHistory(QFileDialogH handle, const QStringListH paths);
C_EXPORT void QFileDialog_history(QFileDialogH handle, QStringListH retval);
C_EXPORT void QFileDialog_setItemDelegate(QFileDialogH handle, QAbstractItemDelegateH delegate);
C_EXPORT QAbstractItemDelegateH QFileDialog_itemDelegate(QFileDialogH handle);
C_EXPORT void QFileDialog_setIconProvider(QFileDialogH handle, QFileIconProviderH provider);
C_EXPORT QFileIconProviderH QFileDialog_iconProvider(QFileDialogH handle);
C_EXPORT void QFileDialog_setLabelText(QFileDialogH handle, QFileDialog::DialogLabel label, PWideString text);
C_EXPORT void QFileDialog_labelText(QFileDialogH handle, PWideString retval, QFileDialog::DialogLabel label);
C_EXPORT void QFileDialog_setProxyModel(QFileDialogH handle, QAbstractProxyModelH model);
C_EXPORT QAbstractProxyModelH QFileDialog_proxyModel(QFileDialogH handle);
C_EXPORT void QFileDialog_setOption(QFileDialogH handle, QFileDialog::Option option, bool on);
C_EXPORT bool QFileDialog_testOption(QFileDialogH handle, QFileDialog::Option option);
C_EXPORT void QFileDialog_setOptions(QFileDialogH handle, unsigned int options);
C_EXPORT unsigned int QFileDialog_options(QFileDialogH handle);
C_EXPORT void QFileDialog_open(QFileDialogH handle, QObjectH receiver, const char* member);
C_EXPORT void QFileDialog_setVisible(QFileDialogH handle, bool visible);

C_EXPORT void QFileDialog_setMimeTypeFilters(QFileDialogH handle, const QStringListH filters);
C_EXPORT void QFileDialog_mimeTypeFilters(QFileDialogH handle, QStringListH retval);
C_EXPORT void QFileDialog_selectedMimeTypeFilter(QFileDialogH handle, PWideString retval);
C_EXPORT void QFileDialog_selectMimeTypeFilter(QFileDialogH handle, PWideString filter);
C_EXPORT void QFileDialog_selectUrl(QFileDialogH handle, const QUrlH url);
C_EXPORT void QFileDialog_selectedUrls(QFileDialogH handle, PPtrIntArray retval);
C_EXPORT void QFileDialog_directoryUrl(QFileDialogH handle, QUrlH retval);
C_EXPORT void QFileDialog_setDirectoryUrl(QFileDialogH handle, const QUrlH directory);
C_EXPORT void QFileDialog_supportedSchemes(QFileDialogH handle, QStringListH retval);
C_EXPORT void QFileDialog_setSupportedSchemes(QFileDialogH handle, const QStringListH schemes);

C_EXPORT void QFileDialog_getOpenFileName(PWideString retval, QWidgetH parent, PWideString caption, PWideString dir, PWideString filter, PWideString selectedFilter, unsigned int options);
C_EXPORT void QFileDialog_getSaveFileName(PWideString retval, QWidgetH parent, PWideString caption, PWideString dir, PWideString filter, PWideString selectedFilter, unsigned int options);
C_EXPORT void QFileDialog_getExistingDirectory(PWideString retval, QWidgetH parent, PWideString caption, PWideString dir, unsigned int options);
C_EXPORT void QFileDialog_getOpenFileNames(QStringListH retval, QWidgetH parent, PWideString caption, PWideString dir, PWideString filter, PWideString selectedFilter, unsigned int options);

#endif
