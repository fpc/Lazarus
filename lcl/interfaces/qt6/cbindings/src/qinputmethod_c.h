//******************************************************************************
//  Copyright (c) 2024 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QINPUTMETHOD_C_H
#define QINPUTMETHOD_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QInputMethod_anchorRectangle(QInputMethodH handle, QRectFH retval);
C_EXPORT void QInputMethod_cursorRectangle(QInputMethodH handle, QRectFH retval);
C_EXPORT Qt::LayoutDirection QInputMethod_inputDirection(QInputMethodH handle);
C_EXPORT void QInputMethod_inputItemClipRectangle(QInputMethodH handle, QRectFH retval);
C_EXPORT void QInputMethod_inputItemRectangle(QInputMethodH handle, QRectFH retval);
C_EXPORT void QInputMethod_inputItemTransform(QInputMethodH handle, QTransformH retval);
C_EXPORT bool QInputMethod_isAnimating(QInputMethodH handle);
C_EXPORT void QInputMethod_keyboardRectangle(QInputMethodH handle, QRectFH retval);
C_EXPORT void QInputMethod_locale(QInputMethodH handle, QLocaleH retval);
C_EXPORT void QInputMethod_setInputItemRectangle(QInputMethodH handle, QRectFH rect);
C_EXPORT void QInputMethod_setInputItemTransform(QInputMethodH handle, QTransformH transform);
C_EXPORT bool QInputMethod_isVisible(QInputMethodH handle);
C_EXPORT void QInputMethod_setVisible(QInputMethodH handle, bool visible);
C_EXPORT void QInputMethod_commit(QInputMethodH handle);
C_EXPORT void QInputMethod_hide(QInputMethodH handle);
C_EXPORT void QInputMethod_invokeAction(QInputMethodH handle, QInputMethod::Action a, int cursorPosition);
C_EXPORT void QInputMethod_reset(QInputMethodH handle);
C_EXPORT void QInputMethod_show(QInputMethodH handle);
C_EXPORT void QInputMethod_update(QInputMethodH handle, Qt::InputMethodQueries queries);

#endif
