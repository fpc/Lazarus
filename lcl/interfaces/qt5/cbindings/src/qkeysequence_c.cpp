//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qkeysequence_c.h"

QKeySequenceH QKeySequence_Create()
{
	return (QKeySequenceH) new QKeySequence();
}

void QKeySequence_Destroy(QKeySequenceH handle)
{
	delete (QKeySequence *)handle;
}

QKeySequenceH QKeySequence_Create2(PWideString key, QKeySequence::SequenceFormat format)
{
	QString t_key;
	copyPWideStringToQString(key, t_key);
	return (QKeySequenceH) new QKeySequence(t_key, format);
}

QKeySequenceH QKeySequence_Create3(int k1, int k2, int k3, int k4)
{
	return (QKeySequenceH) new QKeySequence(k1, k2, k3, k4);
}

QKeySequenceH QKeySequence_Create4(const QKeySequenceH ks)
{
	return (QKeySequenceH) new QKeySequence(*(const QKeySequence*)ks);
}

QKeySequenceH QKeySequence_Create5(QKeySequence::StandardKey key)
{
	return (QKeySequenceH) new QKeySequence(key);
}

int QKeySequence_count(QKeySequenceH handle)
{
	return (int) ((QKeySequence *)handle)->count();
}

bool QKeySequence_isEmpty(QKeySequenceH handle)
{
	return (bool) ((QKeySequence *)handle)->isEmpty();
}

void QKeySequence_toString(QKeySequenceH handle, PWideString retval, QKeySequence::SequenceFormat format)
{
	QString t_retval;
	t_retval = ((QKeySequence *)handle)->toString(format);
	copyQStringToPWideString(t_retval, retval);
}

void QKeySequence_fromString(QKeySequenceH retval, PWideString str, QKeySequence::SequenceFormat format)
{
	QString t_str;
	copyPWideStringToQString(str, t_str);
	*(QKeySequence *)retval = QKeySequence::fromString(t_str, format);
}

QKeySequence::SequenceMatch QKeySequence_matches(QKeySequenceH handle, const QKeySequenceH seq)
{
	return (QKeySequence::SequenceMatch) ((QKeySequence *)handle)->matches(*(const QKeySequence*)seq);
}

void QKeySequence_mnemonic(QKeySequenceH retval, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	*(QKeySequence *)retval = QKeySequence::mnemonic(t_text);
}

void QKeySequence_swap(QKeySequenceH handle, QKeySequenceH other)
{
	((QKeySequence *)handle)->swap(*(QKeySequence*)other);
}

bool QKeySequence_isDetached(QKeySequenceH handle)
{
	return (bool) ((QKeySequence *)handle)->isDetached();
}

