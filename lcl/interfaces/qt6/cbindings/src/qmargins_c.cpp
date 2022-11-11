//******************************************************************************
//  Copyright (c) 2017-2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmargins_c.h"

QMarginsH QMargins_Create()
{
	return (QMarginsH) new QMargins();
}

QMarginsH QMargins_Create2(int left, int top, int right, int bottom)
{
  return (QMarginsH) new QMargins(left, top, right, bottom);
}

void QMargins_Destroy(QMarginsH handle)
{
	delete (QMargins *)handle;
}

bool QMargins_isNull(QMarginsH handle)
{
  return (bool) ((QMargins *)handle)->isNull();
}

int QMargins_left(QMarginsH handle)
{
  return (int) ((QMargins *)handle)->left();
}

int QMargins_top(QMarginsH handle)
{
  return (int) ((QMargins *)handle)->top();
}

int QMargins_right(QMarginsH handle)
{
  return (int) ((QMargins *)handle)->right();
}

int QMargins_bottom(QMarginsH handle)
{
  return (int) ((QMargins *)handle)->bottom();
}

void QMargins_setLeft(QMarginsH handle, int left)
{
  ((QMargins *)handle)->setLeft(left);
}

void QMargins_setTop(QMarginsH handle, int top)
{
  ((QMargins *)handle)->setTop(top);
}

void QMargins_setRight(QMarginsH handle, int right)
{
  ((QMargins *)handle)->setRight(right);
}

void QMargins_setBottom(QMarginsH handle, int bottom)
{
  ((QMargins *)handle)->setBottom(bottom);
}


QMarginsFH QMarginsF_Create()
{
	return (QMarginsFH) new QMarginsF();
}

QMarginsFH QMarginsF_Create2(qreal left, qreal top, qreal right, qreal bottom)
{
  return (QMarginsFH) new QMarginsF(left, top, right, bottom);
}

void QMarginsF_Destroy(QMarginsFH handle)
{
	delete (QMarginsF *)handle;
}

bool QMarginsF_isNull(QMarginsFH handle)
{
  return (bool) ((QMarginsF *)handle)->isNull();
}

qreal QMarginsF_left(QMarginsFH handle)
{
  return (int) ((QMarginsF *)handle)->left();
}

qreal QMarginsF_top(QMarginsFH handle)
{
  return (int) ((QMarginsF *)handle)->top();
}

qreal QMarginsF_right(QMarginsFH handle)
{
  return (int) ((QMarginsF *)handle)->right();
}

qreal QMarginsF_bottom(QMarginsFH handle)
{
  return (int) ((QMarginsF *)handle)->bottom();
}

void QMarginsF_setLeft(QMarginsFH handle, qreal left)
{
  ((QMarginsF *)handle)->setLeft(left);
}

void QMarginsF_setTop(QMarginsFH handle, qreal top)
{
  ((QMarginsF *)handle)->setTop(top);
}

void QMarginsF_setRight(QMarginsFH handle, qreal right)
{
  ((QMarginsF *)handle)->setRight(right);
}

void QMarginsF_setBottom(QMarginsFH handle, qreal bottom)
{
  ((QMarginsF *)handle)->setBottom(bottom);
}

void QMarginsF_toMargins(QMarginsFH handle, QMarginsH retval)
{
  *(QMargins*)retval = ((QMarginsF *)handle)->toMargins();
}


