//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOLORDIALOG_C_H
#define QCOLORDIALOG_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QColorDialogH QColorDialog_Create(QWidgetH parent);
C_EXPORT void QColorDialog_Destroy(QColorDialogH handle);
C_EXPORT QColorDialogH QColorDialog_Create2(const QColorH initial, QWidgetH parent);
C_EXPORT void QColorDialog_setCurrentColor(QColorDialogH handle, const QColorH color);
C_EXPORT void QColorDialog_currentColor(QColorDialogH handle, PQColor retval);
C_EXPORT void QColorDialog_selectedColor(QColorDialogH handle, PQColor retval);
C_EXPORT void QColorDialog_setOption(QColorDialogH handle, QColorDialog::ColorDialogOption option, bool on);
C_EXPORT bool QColorDialog_testOption(QColorDialogH handle, QColorDialog::ColorDialogOption option);
C_EXPORT void QColorDialog_setOptions(QColorDialogH handle, unsigned int options);
C_EXPORT unsigned int QColorDialog_options(QColorDialogH handle);
C_EXPORT void QColorDialog_open(QColorDialogH handle, QObjectH receiver, const char* member);
C_EXPORT void QColorDialog_setVisible(QColorDialogH handle, bool visible);
C_EXPORT bool QColorDialog_getColor(PQColor retval, const QColorH initial, QWidgetH parent, PWideString title, unsigned int options);
C_EXPORT int QColorDialog_customCount();
C_EXPORT void QColorDialog_customColor(PQColor retval, int index);
C_EXPORT void QColorDialog_setCustomColor(int index, PQColor color);
C_EXPORT void QColorDialog_standardColor(PQColor retval, int index);
C_EXPORT void QColorDialog_setStandardColor(int index, PQColor color);

#endif
