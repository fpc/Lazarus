//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QACTIONGROUP_C_H
#define QACTIONGROUP_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QActionGroupH QActionGroup_Create(QObjectH parent);
C_EXPORT void QActionGroup_Destroy(QActionGroupH handle);
C_EXPORT QActionH QActionGroup_addAction(QActionGroupH handle, QActionH a);
C_EXPORT QActionH QActionGroup_addAction2(QActionGroupH handle, PWideString text);
C_EXPORT QActionH QActionGroup_addAction3(QActionGroupH handle, const QIconH icon, PWideString text);
C_EXPORT void QActionGroup_removeAction(QActionGroupH handle, QActionH a);
C_EXPORT void QActionGroup_actions(QActionGroupH handle, PPtrIntArray retval);
C_EXPORT QActionH QActionGroup_checkedAction(QActionGroupH handle);
C_EXPORT bool QActionGroup_isExclusive(QActionGroupH handle);
C_EXPORT bool QActionGroup_isEnabled(QActionGroupH handle);
C_EXPORT bool QActionGroup_isVisible(QActionGroupH handle);
C_EXPORT void QActionGroup_setEnabled(QActionGroupH handle, bool AnonParam1);
C_EXPORT void QActionGroup_setDisabled(QActionGroupH handle, bool b);
C_EXPORT void QActionGroup_setVisible(QActionGroupH handle, bool AnonParam1);
C_EXPORT void QActionGroup_setExclusive(QActionGroupH handle, bool AnonParam1);

#endif
