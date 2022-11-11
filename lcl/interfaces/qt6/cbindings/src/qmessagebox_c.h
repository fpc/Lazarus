//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMESSAGEBOX_C_H
#define QMESSAGEBOX_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QMessageBoxH QMessageBox_Create(QWidgetH parent);
C_EXPORT void QMessageBox_Destroy(QMessageBoxH handle);
C_EXPORT QMessageBoxH QMessageBox_Create2(QMessageBox::Icon icon, PWideString title, PWideString text, unsigned int buttons, QWidgetH parent, unsigned int flags);
C_EXPORT void QMessageBox_addButton(QMessageBoxH handle, QAbstractButtonH button, QMessageBox::ButtonRole role);
C_EXPORT QPushButtonH QMessageBox_addButton2(QMessageBoxH handle, PWideString text, QMessageBox::ButtonRole role);
C_EXPORT QPushButtonH QMessageBox_addButton3(QMessageBoxH handle, QMessageBox::StandardButton button);
C_EXPORT void QMessageBox_removeButton(QMessageBoxH handle, QAbstractButtonH button);
C_EXPORT void QMessageBox_open(QMessageBoxH handle, QObjectH receiver, const char* member);
C_EXPORT void QMessageBox_buttons(QMessageBoxH handle, PPtrIntArray retval);
C_EXPORT QMessageBox::ButtonRole QMessageBox_buttonRole(QMessageBoxH handle, QAbstractButtonH button);
C_EXPORT void QMessageBox_setStandardButtons(QMessageBoxH handle, unsigned int buttons);
C_EXPORT unsigned int QMessageBox_standardButtons(QMessageBoxH handle);
C_EXPORT QMessageBox::StandardButton QMessageBox_standardButton(QMessageBoxH handle, QAbstractButtonH button);
C_EXPORT QAbstractButtonH QMessageBox_button(QMessageBoxH handle, QMessageBox::StandardButton which);
C_EXPORT QPushButtonH QMessageBox_defaultButton(QMessageBoxH handle);
C_EXPORT void QMessageBox_setDefaultButton(QMessageBoxH handle, QPushButtonH button);
C_EXPORT void QMessageBox_setDefaultButton2(QMessageBoxH handle, QMessageBox::StandardButton button);
C_EXPORT QAbstractButtonH QMessageBox_escapeButton(QMessageBoxH handle);
C_EXPORT void QMessageBox_setEscapeButton(QMessageBoxH handle, QAbstractButtonH button);
C_EXPORT void QMessageBox_setEscapeButton2(QMessageBoxH handle, QMessageBox::StandardButton button);
C_EXPORT QAbstractButtonH QMessageBox_clickedButton(QMessageBoxH handle);
C_EXPORT void QMessageBox_text(QMessageBoxH handle, PWideString retval);
C_EXPORT void QMessageBox_setText(QMessageBoxH handle, PWideString text);
C_EXPORT QMessageBox::Icon QMessageBox_icon(QMessageBoxH handle);
C_EXPORT void QMessageBox_setIcon(QMessageBoxH handle, QMessageBox::Icon AnonParam1);
C_EXPORT void QMessageBox_iconPixmap(QMessageBoxH handle, QPixmapH retval);
C_EXPORT void QMessageBox_setIconPixmap(QMessageBoxH handle, const QPixmapH pixmap);
C_EXPORT Qt::TextFormat QMessageBox_textFormat(QMessageBoxH handle);
C_EXPORT void QMessageBox_setTextFormat(QMessageBoxH handle, Qt::TextFormat format);
C_EXPORT void QMessageBox_setTextInteractionFlags(QMessageBoxH handle, unsigned int flags);
C_EXPORT unsigned int QMessageBox_textInteractionFlags(QMessageBoxH handle);
C_EXPORT QCheckBoxH QMessageBox_checkBox(QMessageBoxH handle);
C_EXPORT void QMessageBox_setCheckBox(QMessageBoxH handle, QCheckBoxH checkbox);

C_EXPORT QMessageBox::StandardButton QMessageBox_information(QWidgetH parent, PWideString title, PWideString text, unsigned int buttons, QMessageBox::StandardButton defaultButton);
C_EXPORT QMessageBox::StandardButton QMessageBox_question(QWidgetH parent, PWideString title, PWideString text, unsigned int buttons, QMessageBox::StandardButton defaultButton);
C_EXPORT QMessageBox::StandardButton QMessageBox_warning(QWidgetH parent, PWideString title, PWideString text, unsigned int buttons, QMessageBox::StandardButton defaultButton);
C_EXPORT QMessageBox::StandardButton QMessageBox_critical(QWidgetH parent, PWideString title, PWideString text, unsigned int buttons, QMessageBox::StandardButton defaultButton);
C_EXPORT void QMessageBox_about(QWidgetH parent, PWideString title, PWideString text);
C_EXPORT void QMessageBox_aboutQt(QWidgetH parent, PWideString title);
C_EXPORT QMessageBoxH QMessageBox_Create3(PWideString title, PWideString text, QMessageBox::Icon icon, int button0, int button1, int button2, QWidgetH parent, unsigned int f);
C_EXPORT int QMessageBox_information3(QWidgetH parent, PWideString title, PWideString text, PWideString button0Text, PWideString button1Text, PWideString button2Text, int defaultButtonNumber, int escapeButtonNumber);
C_EXPORT int QMessageBox_question3(QWidgetH parent, PWideString title, PWideString text, PWideString button0Text, PWideString button1Text, PWideString button2Text, int defaultButtonNumber, int escapeButtonNumber);
C_EXPORT int QMessageBox_warning3(QWidgetH parent, PWideString title, PWideString text, PWideString button0Text, PWideString button1Text, PWideString button2Text, int defaultButtonNumber, int escapeButtonNumber);
C_EXPORT int QMessageBox_critical3(QWidgetH parent, PWideString title, PWideString text, PWideString button0Text, PWideString button1Text, PWideString button2Text, int defaultButtonNumber, int escapeButtonNumber);
C_EXPORT void QMessageBox_buttonText(QMessageBoxH handle, PWideString retval, int button);
C_EXPORT void QMessageBox_setButtonText(QMessageBoxH handle, int button, PWideString text);
C_EXPORT void QMessageBox_informativeText(QMessageBoxH handle, PWideString retval);
C_EXPORT void QMessageBox_setInformativeText(QMessageBoxH handle, PWideString text);
C_EXPORT void QMessageBox_detailedText(QMessageBoxH handle, PWideString retval);
C_EXPORT void QMessageBox_setDetailedText(QMessageBoxH handle, PWideString text);
C_EXPORT void QMessageBox_setWindowTitle(QMessageBoxH handle, PWideString title);
C_EXPORT void QMessageBox_setWindowModality(QMessageBoxH handle, Qt::WindowModality windowModality);
C_EXPORT void QMessageBox_standardIcon(QPixmapH retval, QMessageBox::Icon icon);

#endif
