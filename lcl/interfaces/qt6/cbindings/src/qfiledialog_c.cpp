//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfiledialog_c.h"

QFileDialogH QFileDialog_Create(QWidgetH parent, unsigned int f)
{
	return (QFileDialogH) new QFileDialog((QWidget*)parent, (Qt::WindowFlags)f);
}

void QFileDialog_Destroy(QFileDialogH handle)
{
	delete (QFileDialog *)handle;
}

QFileDialogH QFileDialog_Create2(QWidgetH parent, PWideString caption, PWideString directory, PWideString filter)
{
	QString t_caption;
	QString t_directory;
	QString t_filter;
	copyPWideStringToQString(caption, t_caption);
	copyPWideStringToQString(directory, t_directory);
	copyPWideStringToQString(filter, t_filter);
	return (QFileDialogH) new QFileDialog((QWidget*)parent, t_caption, t_directory, t_filter);
}

void QFileDialog_setDirectory(QFileDialogH handle, PWideString directory)
{
	QString t_directory;
	copyPWideStringToQString(directory, t_directory);
	((QFileDialog *)handle)->setDirectory(t_directory);
}

void QFileDialog_setDirectory2(QFileDialogH handle, const QDirH directory)
{
	((QFileDialog *)handle)->setDirectory(*(const QDir*)directory);
}

void QFileDialog_directory(QFileDialogH handle, QDirH retval)
{
	*(QDir *)retval = ((QFileDialog *)handle)->directory();
}

void QFileDialog_selectFile(QFileDialogH handle, PWideString filename)
{
	QString t_filename;
	copyPWideStringToQString(filename, t_filename);
	((QFileDialog *)handle)->selectFile(t_filename);
}

void QFileDialog_selectedFiles(QFileDialogH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QFileDialog *)handle)->selectedFiles();
}

void QFileDialog_setNameFilter(QFileDialogH handle, PWideString filter)
{
	QString t_filter;
	copyPWideStringToQString(filter, t_filter);
	((QFileDialog *)handle)->setNameFilter(t_filter);
}

void QFileDialog_setNameFilters(QFileDialogH handle, const QStringListH filters)
{
	((QFileDialog *)handle)->setNameFilters(*(const QStringList*)filters);
}

void QFileDialog_nameFilters(QFileDialogH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QFileDialog *)handle)->nameFilters();
}

void QFileDialog_selectNameFilter(QFileDialogH handle, PWideString filter)
{
	QString t_filter;
	copyPWideStringToQString(filter, t_filter);
	((QFileDialog *)handle)->selectNameFilter(t_filter);
}

void QFileDialog_selectedNameFilter(QFileDialogH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileDialog *)handle)->selectedNameFilter();
	copyQStringToPWideString(t_retval, retval);
}

unsigned int QFileDialog_filter(QFileDialogH handle)
{
	return (unsigned int) ((QFileDialog *)handle)->filter();
}

void QFileDialog_setFilter(QFileDialogH handle, unsigned int filters)
{
	((QFileDialog *)handle)->setFilter((QDir::Filters)filters);
}

void QFileDialog_setViewMode(QFileDialogH handle, QFileDialog::ViewMode mode)
{
	((QFileDialog *)handle)->setViewMode(mode);
}

QFileDialog::ViewMode QFileDialog_viewMode(QFileDialogH handle)
{
	return (QFileDialog::ViewMode) ((QFileDialog *)handle)->viewMode();
}

void QFileDialog_setFileMode(QFileDialogH handle, QFileDialog::FileMode mode)
{
	((QFileDialog *)handle)->setFileMode(mode);
}

QFileDialog::FileMode QFileDialog_fileMode(QFileDialogH handle)
{
	return (QFileDialog::FileMode) ((QFileDialog *)handle)->fileMode();
}

void QFileDialog_setAcceptMode(QFileDialogH handle, QFileDialog::AcceptMode mode)
{
	((QFileDialog *)handle)->setAcceptMode(mode);
}

QFileDialog::AcceptMode QFileDialog_acceptMode(QFileDialogH handle)
{
	return (QFileDialog::AcceptMode) ((QFileDialog *)handle)->acceptMode();
}

void QFileDialog_saveState(QFileDialogH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QFileDialog *)handle)->saveState();
}

bool QFileDialog_restoreState(QFileDialogH handle, const QByteArrayH state)
{
	return (bool) ((QFileDialog *)handle)->restoreState(*(const QByteArray*)state);
}

void QFileDialog_setDefaultSuffix(QFileDialogH handle, PWideString suffix)
{
	QString t_suffix;
	copyPWideStringToQString(suffix, t_suffix);
	((QFileDialog *)handle)->setDefaultSuffix(t_suffix);
}

void QFileDialog_defaultSuffix(QFileDialogH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileDialog *)handle)->defaultSuffix();
	copyQStringToPWideString(t_retval, retval);
}

void QFileDialog_setHistory(QFileDialogH handle, const QStringListH paths)
{
	((QFileDialog *)handle)->setHistory(*(const QStringList*)paths);
}

void QFileDialog_history(QFileDialogH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QFileDialog *)handle)->history();
}

void QFileDialog_setItemDelegate(QFileDialogH handle, QAbstractItemDelegateH delegate)
{
	((QFileDialog *)handle)->setItemDelegate((QAbstractItemDelegate*)delegate);
}

QAbstractItemDelegateH QFileDialog_itemDelegate(QFileDialogH handle)
{
	return (QAbstractItemDelegateH) ((QFileDialog *)handle)->itemDelegate();
}

void QFileDialog_setIconProvider(QFileDialogH handle, QFileIconProviderH provider)
{
	((QFileDialog *)handle)->setIconProvider((QFileIconProvider*)provider);
}

QFileIconProviderH QFileDialog_iconProvider(QFileDialogH handle)
{
	return (QFileIconProviderH) ((QFileDialog *)handle)->iconProvider();
}

void QFileDialog_setLabelText(QFileDialogH handle, QFileDialog::DialogLabel label, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QFileDialog *)handle)->setLabelText(label, t_text);
}

void QFileDialog_labelText(QFileDialogH handle, PWideString retval, QFileDialog::DialogLabel label)
{
	QString t_retval;
	t_retval = ((QFileDialog *)handle)->labelText(label);
	copyQStringToPWideString(t_retval, retval);
}

void QFileDialog_setProxyModel(QFileDialogH handle, QAbstractProxyModelH model)
{
	((QFileDialog *)handle)->setProxyModel((QAbstractProxyModel*)model);
}

QAbstractProxyModelH QFileDialog_proxyModel(QFileDialogH handle)
{
	return (QAbstractProxyModelH) ((QFileDialog *)handle)->proxyModel();
}

void QFileDialog_setOption(QFileDialogH handle, QFileDialog::Option option, bool on)
{
	((QFileDialog *)handle)->setOption(option, on);
}

bool QFileDialog_testOption(QFileDialogH handle, QFileDialog::Option option)
{
	return (bool) ((QFileDialog *)handle)->testOption(option);
}

void QFileDialog_setOptions(QFileDialogH handle, unsigned int options)
{
	((QFileDialog *)handle)->setOptions((QFileDialog::Options)options);
}

unsigned int QFileDialog_options(QFileDialogH handle)
{
	return (unsigned int) ((QFileDialog *)handle)->options();
}

void QFileDialog_open(QFileDialogH handle, QObjectH receiver, const char* member)
{
	((QFileDialog *)handle)->open((QObject*)receiver, member);
}

void QFileDialog_setVisible(QFileDialogH handle, bool visible)
{
	((QFileDialog *)handle)->setVisible(visible);
}

void QFileDialog_setMimeTypeFilters(QFileDialogH handle, const QStringListH filters)
{
	((QFileDialog *)handle)->setMimeTypeFilters(*(QStringList *)filters);
}
void QFileDialog_mimeTypeFilters(QFileDialogH handle, QStringListH retval)
{
  *(QStringList *)retval = ((QFileDialog *)handle)->mimeTypeFilters();
}

void QFileDialog_selectedMimeTypeFilter(QFileDialogH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileDialog *)handle)->selectedMimeTypeFilter();
	copyQStringToPWideString(t_retval, retval);
}

void QFileDialog_selectMimeTypeFilter(QFileDialogH handle, PWideString filter)
{
	QString t_filter;
	copyPWideStringToQString(filter, t_filter);
	((QFileDialog *)handle)->selectMimeTypeFilter(t_filter);
}

void QFileDialog_selectUrl(QFileDialogH handle, const QUrlH url)
{
  ((QFileDialog *)handle)->selectUrl(*(const QUrl *)url);
}

void QFileDialog_selectedUrls(QFileDialogH handle, PPtrIntArray retval)
{
	QList<QUrl> t_retval;
	t_retval = ((QFileDialog *)handle)->selectedUrls();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QFileDialog_directoryUrl(QFileDialogH handle, QUrlH retval)
{
  *(QUrl *)retval = ((QFileDialog *)handle)->directoryUrl();
}

void QFileDialog_setDirectoryUrl(QFileDialogH handle, const QUrlH directory)
{
  ((QFileDialog *)handle)->setDirectoryUrl(*(const QUrl*)directory);
}

void QFileDialog_supportedSchemes(QFileDialogH handle, QStringListH retval)
{
 *(QStringList *)retval = ((QFileDialog *)handle)->supportedSchemes();
}

void QFileDialog_setSupportedSchemes(QFileDialogH handle, const QStringListH schemes)
{
  ((QFileDialog *)handle)->setSupportedSchemes(*(const QStringList*)schemes);
}

void QFileDialog_getOpenFileName(PWideString retval, QWidgetH parent, PWideString caption, PWideString dir, PWideString filter, PWideString selectedFilter, unsigned int options)
{
	QString t_retval;
	QString t_caption;
	QString t_dir;
	QString t_filter;
	QString t_selectedFilter;
	copyPWideStringToQString(caption, t_caption);
	copyPWideStringToQString(dir, t_dir);
	copyPWideStringToQString(filter, t_filter);
	if ( selectedFilter )
		copyPWideStringToQString(selectedFilter, t_selectedFilter);
	t_retval = QFileDialog::getOpenFileName((QWidget*)parent, t_caption, t_dir, t_filter, selectedFilter ? &t_selectedFilter : NULL, (QFileDialog::Options)options);
	copyQStringToPWideString(t_retval, retval);
	if ( selectedFilter )
		copyQStringToPWideString(t_selectedFilter, selectedFilter);
}

void QFileDialog_getSaveFileName(PWideString retval, QWidgetH parent, PWideString caption, PWideString dir, PWideString filter, PWideString selectedFilter, unsigned int options)
{
	QString t_retval;
	QString t_caption;
	QString t_dir;
	QString t_filter;
	QString t_selectedFilter;
	copyPWideStringToQString(caption, t_caption);
	copyPWideStringToQString(dir, t_dir);
	copyPWideStringToQString(filter, t_filter);
	if ( selectedFilter )
		copyPWideStringToQString(selectedFilter, t_selectedFilter);
	t_retval = QFileDialog::getSaveFileName((QWidget*)parent, t_caption, t_dir, t_filter, selectedFilter ? &t_selectedFilter : NULL, (QFileDialog::Options)options);
	copyQStringToPWideString(t_retval, retval);
	if ( selectedFilter )
		copyQStringToPWideString(t_selectedFilter, selectedFilter);
}

void QFileDialog_getExistingDirectory(PWideString retval, QWidgetH parent, PWideString caption, PWideString dir, unsigned int options)
{
	QString t_retval;
	QString t_caption;
	QString t_dir;
	copyPWideStringToQString(caption, t_caption);
	copyPWideStringToQString(dir, t_dir);
	t_retval = QFileDialog::getExistingDirectory((QWidget*)parent, t_caption, t_dir, (QFileDialog::Options)options);
	copyQStringToPWideString(t_retval, retval);
}

void QFileDialog_getOpenFileNames(QStringListH retval, QWidgetH parent, PWideString caption, PWideString dir, PWideString filter, PWideString selectedFilter, unsigned int options)
{
	QString t_caption;
	QString t_dir;
	QString t_filter;
	QString t_selectedFilter;
	copyPWideStringToQString(caption, t_caption);
	copyPWideStringToQString(dir, t_dir);
	copyPWideStringToQString(filter, t_filter);
	if ( selectedFilter )
		copyPWideStringToQString(selectedFilter, t_selectedFilter);
	*(QStringList *)retval = QFileDialog::getOpenFileNames((QWidget*)parent, t_caption, t_dir, t_filter, selectedFilter ? &t_selectedFilter : NULL, (QFileDialog::Options)options);
	if ( selectedFilter )
		copyQStringToPWideString(t_selectedFilter, selectedFilter);
}

