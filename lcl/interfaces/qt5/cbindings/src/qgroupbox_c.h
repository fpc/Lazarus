//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGROUPBOX_C_H
#define QGROUPBOX_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QGroupBoxH QGroupBox_Create(QWidgetH parent);
C_EXPORT void QGroupBox_Destroy(QGroupBoxH handle);
C_EXPORT QGroupBoxH QGroupBox_Create2(PWideString title, QWidgetH parent);
C_EXPORT void QGroupBox_title(QGroupBoxH handle, PWideString retval);
C_EXPORT void QGroupBox_setTitle(QGroupBoxH handle, PWideString title);
C_EXPORT unsigned int QGroupBox_alignment(QGroupBoxH handle);
C_EXPORT void QGroupBox_setAlignment(QGroupBoxH handle, int alignment);
C_EXPORT void QGroupBox_minimumSizeHint(QGroupBoxH handle, PSize retval);
C_EXPORT bool QGroupBox_isFlat(QGroupBoxH handle);
C_EXPORT void QGroupBox_setFlat(QGroupBoxH handle, bool flat);
C_EXPORT bool QGroupBox_isCheckable(QGroupBoxH handle);
C_EXPORT void QGroupBox_setCheckable(QGroupBoxH handle, bool checkable);
C_EXPORT bool QGroupBox_isChecked(QGroupBoxH handle);
C_EXPORT void QGroupBox_setChecked(QGroupBoxH handle, bool checked);

#endif
