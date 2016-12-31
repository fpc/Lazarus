//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qgesturerecognizer_c.h"

QGestureH QGestureRecognizer_create(QGestureRecognizerH handle, QObjectH target)
{
	return (QGestureH) ((QGestureRecognizer *)handle)->create((QObject*)target);
}

unsigned int QGestureRecognizer_recognize(QGestureRecognizerH handle, QGestureH state, QObjectH watched, QEventH event)
{
	return (unsigned int) ((QGestureRecognizer *)handle)->recognize((QGesture*)state, (QObject*)watched, (QEvent*)event);
}

void QGestureRecognizer_reset(QGestureRecognizerH handle, QGestureH state)
{
	((QGestureRecognizer *)handle)->reset((QGesture*)state);
}

Qt::GestureType QGestureRecognizer_registerRecognizer(QGestureRecognizerH recognizer)
{
	return (Qt::GestureType) QGestureRecognizer::registerRecognizer((QGestureRecognizer*)recognizer);
}

void QGestureRecognizer_unregisterRecognizer(Qt::GestureType type)
{
	QGestureRecognizer::unregisterRecognizer(type);
}

