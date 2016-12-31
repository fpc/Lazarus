//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QBUTTONGROUP_C_H
#define QBUTTONGROUP_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QButtonGroupH QButtonGroup_Create(QObjectH parent);
C_EXPORT void QButtonGroup_Destroy(QButtonGroupH handle);
C_EXPORT void QButtonGroup_setExclusive(QButtonGroupH handle, bool AnonParam1);
C_EXPORT bool QButtonGroup_exclusive(QButtonGroupH handle);
C_EXPORT void QButtonGroup_addButton(QButtonGroupH handle, QAbstractButtonH AnonParam1, int id);
C_EXPORT void QButtonGroup_removeButton(QButtonGroupH handle, QAbstractButtonH AnonParam1);
C_EXPORT void QButtonGroup_buttons(QButtonGroupH handle, PPtrIntArray retval);
C_EXPORT QAbstractButtonH QButtonGroup_checkedButton(QButtonGroupH handle);
C_EXPORT QAbstractButtonH QButtonGroup_button(QButtonGroupH handle, int id);
C_EXPORT void QButtonGroup_setId(QButtonGroupH handle, QAbstractButtonH button, int id);
C_EXPORT int QButtonGroup_id(QButtonGroupH handle, QAbstractButtonH button);
C_EXPORT int QButtonGroup_checkedId(QButtonGroupH handle);

#endif
