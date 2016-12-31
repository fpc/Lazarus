//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTOPTION_C_H
#define QTEXTOPTION_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QTextOptionH QTextOption_Create();
C_EXPORT void QTextOption_Destroy(QTextOptionH handle);
C_EXPORT QTextOptionH QTextOption_Create2(unsigned int alignment);
C_EXPORT QTextOptionH QTextOption_Create3(const QTextOptionH o);
C_EXPORT void QTextOption_setAlignment(QTextOptionH handle, unsigned int alignment);
C_EXPORT unsigned int QTextOption_alignment(QTextOptionH handle);
C_EXPORT void QTextOption_setTextDirection(QTextOptionH handle, Qt::LayoutDirection aDirection);
C_EXPORT Qt::LayoutDirection QTextOption_textDirection(QTextOptionH handle);
C_EXPORT void QTextOption_setWrapMode(QTextOptionH handle, QTextOption::WrapMode wrap);
C_EXPORT QTextOption::WrapMode QTextOption_wrapMode(QTextOptionH handle);
C_EXPORT void QTextOption_setFlags(QTextOptionH handle, unsigned int flags);
C_EXPORT unsigned int QTextOption_flags(QTextOptionH handle);
C_EXPORT void QTextOption_setTabStop(QTextOptionH handle, qreal tabStop);
C_EXPORT qreal QTextOption_tabStop(QTextOptionH handle);
C_EXPORT void QTextOption_setTabArray(QTextOptionH handle, PPtrIntArray tabStops);
C_EXPORT void QTextOption_tabArray(QTextOptionH handle, PPtrIntArray retval);
C_EXPORT void QTextOption_setUseDesignMetrics(QTextOptionH handle, bool b);
C_EXPORT bool QTextOption_useDesignMetrics(QTextOptionH handle);

#endif
