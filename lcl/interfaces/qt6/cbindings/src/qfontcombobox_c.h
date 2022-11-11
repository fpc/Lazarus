//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFONTCOMBOBOX_C_H
#define QFONTCOMBOBOX_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QFontComboBoxH QFontComboBox_Create(QWidgetH parent);
C_EXPORT void QFontComboBox_Destroy(QFontComboBoxH handle);
C_EXPORT void QFontComboBox_setWritingSystem(QFontComboBoxH handle, QFontDatabase::WritingSystem AnonParam1);
C_EXPORT QFontDatabase::WritingSystem QFontComboBox_writingSystem(QFontComboBoxH handle);
C_EXPORT void QFontComboBox_setFontFilters(QFontComboBoxH handle, unsigned int filters);
C_EXPORT unsigned int QFontComboBox_fontFilters(QFontComboBoxH handle);
C_EXPORT void QFontComboBox_currentFont(QFontComboBoxH handle, QFontH retval);
C_EXPORT void QFontComboBox_sizeHint(QFontComboBoxH handle, PSize retval);
C_EXPORT void QFontComboBox_setCurrentFont(QFontComboBoxH handle, const QFontH f);

#endif
