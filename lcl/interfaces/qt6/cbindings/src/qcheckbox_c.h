//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCHECKBOX_C_H
#define QCHECKBOX_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QCheckBoxH QCheckBox_Create(QWidgetH parent);
C_EXPORT void QCheckBox_Destroy(QCheckBoxH handle);
C_EXPORT QCheckBoxH QCheckBox_Create2(PWideString text, QWidgetH parent);
C_EXPORT void QCheckBox_sizeHint(QCheckBoxH handle, PSize retval);
C_EXPORT void QCheckBox_minimumSizeHint(QCheckBoxH handle, PSize retval);
C_EXPORT void QCheckBox_setTristate(QCheckBoxH handle, bool y);
C_EXPORT bool QCheckBox_isTristate(QCheckBoxH handle);
C_EXPORT Qt::CheckState QCheckBox_checkState(QCheckBoxH handle);
C_EXPORT void QCheckBox_setCheckState(QCheckBoxH handle, Qt::CheckState state);

#endif
