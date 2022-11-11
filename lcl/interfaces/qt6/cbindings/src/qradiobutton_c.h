//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QRADIOBUTTON_C_H
#define QRADIOBUTTON_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QRadioButtonH QRadioButton_Create(QWidgetH parent);
C_EXPORT void QRadioButton_Destroy(QRadioButtonH handle);
C_EXPORT QRadioButtonH QRadioButton_Create2(PWideString text, QWidgetH parent);
C_EXPORT void QRadioButton_sizeHint(QRadioButtonH handle, PSize retval);
C_EXPORT void QRadioButton_minimumSizeHint(QRadioButtonH handle, PSize retval);

#endif
