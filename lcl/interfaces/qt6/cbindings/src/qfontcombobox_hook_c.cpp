//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfontcombobox_hook_c.h"

QFontComboBox_hookH QFontComboBox_hook_Create(QObjectH handle)
{
	return (QFontComboBox_hookH) new QFontComboBox_hook((QObject*)handle);
}

void QFontComboBox_hook_Destroy(QFontComboBox_hookH handle)
{
	delete (QFontComboBox_hook *)handle;
}

void QFontComboBox_hook_hook_currentFontChanged(QFontComboBox_hookH handle, QHookH hook)
{
	((QFontComboBox_hook *)handle)->hook_currentFontChanged(hook);
}

