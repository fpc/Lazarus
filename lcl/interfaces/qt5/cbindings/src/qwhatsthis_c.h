//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWHATSTHIS_C_H
#define QWHATSTHIS_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT void QWhatsThis_enterWhatsThisMode();
C_EXPORT bool QWhatsThis_inWhatsThisMode();
C_EXPORT void QWhatsThis_leaveWhatsThisMode();
C_EXPORT void QWhatsThis_showText(const QPointH pos, PWideString text, QWidgetH w);
C_EXPORT void QWhatsThis_hideText();
C_EXPORT QActionH QWhatsThis_createAction(QObjectH parent);

#endif
