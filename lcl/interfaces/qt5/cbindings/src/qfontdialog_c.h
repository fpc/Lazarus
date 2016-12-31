//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFONTDIALOG_C_H
#define QFONTDIALOG_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QFontDialogH QFontDialog_Create(QWidgetH parent);
C_EXPORT void QFontDialog_Destroy(QFontDialogH handle);
C_EXPORT QFontDialogH QFontDialog_Create2(const QFontH initial, QWidgetH parent);
C_EXPORT void QFontDialog_setCurrentFont(QFontDialogH handle, const QFontH font);
C_EXPORT void QFontDialog_currentFont(QFontDialogH handle, QFontH retval);
C_EXPORT void QFontDialog_selectedFont(QFontDialogH handle, QFontH retval);
C_EXPORT void QFontDialog_setOption(QFontDialogH handle, QFontDialog::FontDialogOption option, bool on);
C_EXPORT bool QFontDialog_testOption(QFontDialogH handle, QFontDialog::FontDialogOption option);
C_EXPORT void QFontDialog_setOptions(QFontDialogH handle, unsigned int options);
C_EXPORT unsigned int QFontDialog_options(QFontDialogH handle);
C_EXPORT void QFontDialog_open(QFontDialogH handle, QObjectH receiver, const char* member);
C_EXPORT void QFontDialog_setVisible(QFontDialogH handle, bool visible);
C_EXPORT void QFontDialog_getFont(QFontH retval, bool* ok, QWidgetH parent);
C_EXPORT void QFontDialog_getFont2(QFontH retval, bool* ok, const QFontH initial, QWidgetH parent, PWideString title, unsigned int options);

#endif
