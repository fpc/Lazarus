//******************************************************************************
//  Copyright (c) 2024 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qkeycombination_c.h"

QKeyCombinationH QKeyCombination_Create(Qt::Key key)
{
	return (QKeyCombinationH) new QKeyCombination(key);
}

QKeyCombinationH QKeyCombination_Create(Qt::Modifiers modifiers, Qt::Key key)
{
	return (QKeyCombinationH) new QKeyCombination(modifiers, key);
}

QKeyCombinationH QKeyCombination_Create(Qt::KeyboardModifiers modifiers, Qt::Key key)
{
	return (QKeyCombinationH) new QKeyCombination(modifiers, key);
}


void QKeyCombination_Destroy(QKeyCombinationH handle)
{
	delete (QKeyCombination *)handle;
}

Qt::Key QKeyCombination_key(QKeyCombinationH handle)
{
  return (Qt::Key) ((QKeyCombination *)handle)->key();
}

Qt::KeyboardModifiers QKeyCombination_keyboardModifiers(QKeyCombinationH handle)
{
  return (Qt::KeyboardModifiers) ((QKeyCombination *)handle)->keyboardModifiers();
}


int QKeyCombination_toCombined(QKeyCombinationH handle)
{
  return (int) ((QKeyCombination *)handle)->toCombined();
}

void QKeyCombination_fromCombined(int combined, QKeyCombinationH retval)
{
  *(QKeyCombination *) retval = QKeyCombination::fromCombined(combined);
}



