//******************************************************************************
//  Copyright (c) 2024 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QKEYCOMBINATION_C_H
#define QKEYCOMBINATION_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QKeyCombinationH QKeyCombination_Create(Qt::Key key = Qt::Key_unknown);
C_EXPORT QKeyCombinationH QKeyCombination_Create2(Qt::Modifiers modifiers, Qt::Key key = Qt::Key_unknown);
C_EXPORT QKeyCombinationH QKeyCombination_Create3(Qt::KeyboardModifiers modifiers, Qt::Key key = Qt::Key_unknown);
C_EXPORT void QKeyCombination_Destroy(QKeyCombinationH handle);
C_EXPORT Qt::Key QKeyCombination_key(QKeyCombinationH handle);
C_EXPORT Qt::KeyboardModifiers QKeyCombination_keyboardModifiers(QKeyCombinationH handle);
C_EXPORT int QKeyCombination_toCombined(QKeyCombinationH handle);
C_EXPORT void QKeyCombination_fromCombined(int combined, QKeyCombinationH retval);

#endif
