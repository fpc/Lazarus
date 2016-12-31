//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfontcombobox_c.h"

QFontComboBoxH QFontComboBox_Create(QWidgetH parent)
{
	return (QFontComboBoxH) new QFontComboBox((QWidget*)parent);
}

void QFontComboBox_Destroy(QFontComboBoxH handle)
{
	delete (QFontComboBox *)handle;
}

void QFontComboBox_setWritingSystem(QFontComboBoxH handle, QFontDatabase::WritingSystem AnonParam1)
{
	((QFontComboBox *)handle)->setWritingSystem(AnonParam1);
}

QFontDatabase::WritingSystem QFontComboBox_writingSystem(QFontComboBoxH handle)
{
	return (QFontDatabase::WritingSystem) ((QFontComboBox *)handle)->writingSystem();
}

void QFontComboBox_setFontFilters(QFontComboBoxH handle, unsigned int filters)
{
	((QFontComboBox *)handle)->setFontFilters((QFontComboBox::FontFilters)filters);
}

unsigned int QFontComboBox_fontFilters(QFontComboBoxH handle)
{
	return (unsigned int) ((QFontComboBox *)handle)->fontFilters();
}

void QFontComboBox_currentFont(QFontComboBoxH handle, QFontH retval)
{
	*(QFont *)retval = ((QFontComboBox *)handle)->currentFont();
}

void QFontComboBox_sizeHint(QFontComboBoxH handle, PSize retval)
{
	*(QSize *)retval = ((QFontComboBox *)handle)->sizeHint();
}

void QFontComboBox_setCurrentFont(QFontComboBoxH handle, const QFontH f)
{
	((QFontComboBox *)handle)->setCurrentFont(*(const QFont*)f);
}

