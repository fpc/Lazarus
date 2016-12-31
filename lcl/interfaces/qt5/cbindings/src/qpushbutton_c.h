//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPUSHBUTTON_C_H
#define QPUSHBUTTON_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QPushButtonH QPushButton_Create(QWidgetH parent);
C_EXPORT void QPushButton_Destroy(QPushButtonH handle);
C_EXPORT QPushButtonH QPushButton_Create2(PWideString text, QWidgetH parent);
C_EXPORT QPushButtonH QPushButton_Create3(const QIconH icon, PWideString text, QWidgetH parent);
C_EXPORT void QPushButton_sizeHint(QPushButtonH handle, PSize retval);
C_EXPORT void QPushButton_minimumSizeHint(QPushButtonH handle, PSize retval);
C_EXPORT bool QPushButton_autoDefault(QPushButtonH handle);
C_EXPORT void QPushButton_setAutoDefault(QPushButtonH handle, bool AnonParam1);
C_EXPORT bool QPushButton_isDefault(QPushButtonH handle);
C_EXPORT void QPushButton_setDefault(QPushButtonH handle, bool AnonParam1);
C_EXPORT void QPushButton_setMenu(QPushButtonH handle, QMenuH menu);
C_EXPORT QMenuH QPushButton_menu(QPushButtonH handle);
C_EXPORT void QPushButton_setFlat(QPushButtonH handle, bool AnonParam1);
C_EXPORT bool QPushButton_isFlat(QPushButtonH handle);
C_EXPORT void QPushButton_showMenu(QPushButtonH handle);

#endif
