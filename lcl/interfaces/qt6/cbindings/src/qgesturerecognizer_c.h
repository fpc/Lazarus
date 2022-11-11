//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGESTURERECOGNIZER_C_H
#define QGESTURERECOGNIZER_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QGestureH QGestureRecognizer_create(QGestureRecognizerH handle, QObjectH target);
C_EXPORT unsigned int QGestureRecognizer_recognize(QGestureRecognizerH handle, QGestureH state, QObjectH watched, QEventH event);
C_EXPORT void QGestureRecognizer_reset(QGestureRecognizerH handle, QGestureH state);
C_EXPORT Qt::GestureType QGestureRecognizer_registerRecognizer(QGestureRecognizerH recognizer);
C_EXPORT void QGestureRecognizer_unregisterRecognizer(Qt::GestureType type);

#endif
