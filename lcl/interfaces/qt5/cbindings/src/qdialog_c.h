//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDIALOG_C_H
#define QDIALOG_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QDialogH QDialog_Create(QWidgetH parent, unsigned int f);
C_EXPORT void QDialog_Destroy(QDialogH handle);
C_EXPORT int QDialog_result(QDialogH handle);
C_EXPORT void QDialog_setVisible(QDialogH handle, bool visible);
C_EXPORT void QDialog_setOrientation(QDialogH handle, Qt::Orientation orientation);
C_EXPORT Qt::Orientation QDialog_orientation(QDialogH handle);
C_EXPORT void QDialog_setExtension(QDialogH handle, QWidgetH extension);
C_EXPORT QWidgetH QDialog_extension(QDialogH handle);
C_EXPORT void QDialog_sizeHint(QDialogH handle, PSize retval);
C_EXPORT void QDialog_minimumSizeHint(QDialogH handle, PSize retval);
C_EXPORT void QDialog_setSizeGripEnabled(QDialogH handle, bool AnonParam1);
C_EXPORT bool QDialog_isSizeGripEnabled(QDialogH handle);
C_EXPORT void QDialog_setModal(QDialogH handle, bool modal);
C_EXPORT void QDialog_setResult(QDialogH handle, int r);
C_EXPORT void QDialog_open(QDialogH handle);
C_EXPORT int QDialog_exec(QDialogH handle);
C_EXPORT void QDialog_done(QDialogH handle, int AnonParam1);
C_EXPORT void QDialog_accept(QDialogH handle);
C_EXPORT void QDialog_reject(QDialogH handle);
C_EXPORT void QDialog_showExtension(QDialogH handle, bool AnonParam1);

#endif
