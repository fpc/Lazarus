//******************************************************************************
//  Copyright (c) 2024 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qinputmethod_c.h"

void QInputMethod_anchorRectangle(QInputMethodH handle, QRectFH retval)
{
  *(QRectF *)retval = ((QInputMethod *)handle)->anchorRectangle();
}

void QInputMethod_cursorRectangle(QInputMethodH handle, QRectFH retval)
{
  *(QRectF *)retval = ((QInputMethod *)handle)->cursorRectangle();
}

Qt::LayoutDirection QInputMethod_inputDirection(QInputMethodH handle)
{
  return (Qt::LayoutDirection) ((QInputMethod *)handle)->inputDirection();
}

void QInputMethod_inputItemClipRectangle(QInputMethodH handle, QRectFH retval)
{
  *(QRectF *)retval = ((QInputMethod *)handle)->inputItemClipRectangle();
}

void QInputMethod_inputItemRectangle(QInputMethodH handle, QRectFH retval)
{
  *(QRectF *)retval = ((QInputMethod *)handle)->inputItemRectangle();
}

void QInputMethod_inputItemTransform(QInputMethodH handle, QTransformH retval)
{
  *(QTransform *)retval = ((QInputMethod *)handle)->inputItemTransform();
}


bool QInputMethod_isAnimating(QInputMethodH handle)
{
  return (bool) ((QInputMethod *)handle)->isAnimating();
}

void QInputMethod_keyboardRectangle(QInputMethodH handle, QRectFH retval)
{
  *(QRectF *)retval = ((QInputMethod *)handle)->keyboardRectangle();
}

void QInputMethod_locale(QInputMethodH handle, QLocaleH retval)
{
  *(QLocale *)retval = ((QInputMethod *)handle)->locale();
}

void QInputMethod_setInputItemRectangle(QInputMethodH handle, QRectFH rect)
{
  ((QInputMethod *)handle)->setInputItemRectangle(*(const QRectF*)rect);
}

void QInputMethod_setInputItemTransform(QInputMethodH handle, QTransformH transform)
{
  ((QInputMethod *)handle)->setInputItemTransform(*(const QTransform*)transform);
}

bool QInputMethod_isVisible(QInputMethodH handle)
{
  return (bool) ((QInputMethod *)handle)->isVisible();
}

void QInputMethod_setVisible(QInputMethodH handle, bool visible)
{
  ((QInputMethod *)handle)->setVisible(visible);
}

void QInputMethod_commit(QInputMethodH handle)
{
  ((QInputMethod *)handle)->commit();
}

void QInputMethod_hide(QInputMethodH handle)
{
  ((QInputMethod *)handle)->hide();
}

void QInputMethod_invokeAction(QInputMethodH handle, QInputMethod::Action a, int cursorPosition)
{
  ((QInputMethod *)handle)->invokeAction(a, cursorPosition);
}

void QInputMethod_reset(QInputMethodH handle)
{
  ((QInputMethod *)handle)->reset();
}

void QInputMethod_show(QInputMethodH handle)
{
  ((QInputMethod *)handle)->show();
}

void QInputMethod_update(QInputMethodH handle, Qt::InputMethodQueries queries)
{
  ((QInputMethod *)handle)->update(queries);
}



