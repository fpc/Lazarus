//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPUSHBUTTON_HOOK_C_H
#define QPUSHBUTTON_HOOK_C_H

#include "qpushbutton_hook.h"

C_EXPORT QPushButton_hookH QPushButton_hook_Create(QObjectH handle);
C_EXPORT void QPushButton_hook_Destroy(QPushButton_hookH handle);

#endif
