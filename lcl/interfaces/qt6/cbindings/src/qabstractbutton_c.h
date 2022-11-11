//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTBUTTON_C_H
#define QABSTRACTBUTTON_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT void QAbstractButton_setText(QAbstractButtonH handle, PWideString text);
C_EXPORT void QAbstractButton_text(QAbstractButtonH handle, PWideString retval);
C_EXPORT void QAbstractButton_setIcon(QAbstractButtonH handle, const QIconH icon);
C_EXPORT void QAbstractButton_icon(QAbstractButtonH handle, QIconH retval);
C_EXPORT void QAbstractButton_iconSize(QAbstractButtonH handle, PSize retval);
C_EXPORT void QAbstractButton_setShortcut(QAbstractButtonH handle, const QKeySequenceH key);
C_EXPORT void QAbstractButton_shortcut(QAbstractButtonH handle, QKeySequenceH retval);
C_EXPORT void QAbstractButton_setCheckable(QAbstractButtonH handle, bool AnonParam1);
C_EXPORT bool QAbstractButton_isCheckable(QAbstractButtonH handle);
C_EXPORT bool QAbstractButton_isChecked(QAbstractButtonH handle);
C_EXPORT void QAbstractButton_setDown(QAbstractButtonH handle, bool AnonParam1);
C_EXPORT bool QAbstractButton_isDown(QAbstractButtonH handle);
C_EXPORT void QAbstractButton_setAutoRepeat(QAbstractButtonH handle, bool AnonParam1);
C_EXPORT bool QAbstractButton_autoRepeat(QAbstractButtonH handle);
C_EXPORT void QAbstractButton_setAutoRepeatDelay(QAbstractButtonH handle, int AnonParam1);
C_EXPORT int QAbstractButton_autoRepeatDelay(QAbstractButtonH handle);
C_EXPORT void QAbstractButton_setAutoRepeatInterval(QAbstractButtonH handle, int AnonParam1);
C_EXPORT int QAbstractButton_autoRepeatInterval(QAbstractButtonH handle);
C_EXPORT void QAbstractButton_setAutoExclusive(QAbstractButtonH handle, bool AnonParam1);
C_EXPORT bool QAbstractButton_autoExclusive(QAbstractButtonH handle);
C_EXPORT QButtonGroupH QAbstractButton_group(QAbstractButtonH handle);
C_EXPORT void QAbstractButton_setIconSize(QAbstractButtonH handle, const QSizeH size);
C_EXPORT void QAbstractButton_animateClick(QAbstractButtonH handle);
C_EXPORT void QAbstractButton_click(QAbstractButtonH handle);
C_EXPORT void QAbstractButton_toggle(QAbstractButtonH handle);
C_EXPORT void QAbstractButton_setChecked(QAbstractButtonH handle, bool AnonParam1);

#endif
