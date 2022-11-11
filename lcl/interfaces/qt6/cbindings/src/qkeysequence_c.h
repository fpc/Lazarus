//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QKEYSEQUENCE_C_H
#define QKEYSEQUENCE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QKeySequenceH QKeySequence_Create();
C_EXPORT void QKeySequence_Destroy(QKeySequenceH handle);
C_EXPORT QKeySequenceH QKeySequence_Create2(PWideString key, QKeySequence::SequenceFormat format);
C_EXPORT QKeySequenceH QKeySequence_Create3(int k1, int k2, int k3, int k4);
C_EXPORT QKeySequenceH QKeySequence_Create4(const QKeySequenceH ks);
C_EXPORT QKeySequenceH QKeySequence_Create5(QKeySequence::StandardKey key);
C_EXPORT int QKeySequence_count(QKeySequenceH handle);
C_EXPORT bool QKeySequence_isEmpty(QKeySequenceH handle);
C_EXPORT void QKeySequence_toString(QKeySequenceH handle, PWideString retval, QKeySequence::SequenceFormat format);
C_EXPORT void QKeySequence_fromString(QKeySequenceH retval, PWideString str, QKeySequence::SequenceFormat format);
C_EXPORT QKeySequence::SequenceMatch QKeySequence_matches(QKeySequenceH handle, const QKeySequenceH seq);
C_EXPORT void QKeySequence_mnemonic(QKeySequenceH retval, PWideString text);
C_EXPORT void QKeySequence_swap(QKeySequenceH handle, QKeySequenceH other);
C_EXPORT bool QKeySequence_isDetached(QKeySequenceH handle);

#endif
