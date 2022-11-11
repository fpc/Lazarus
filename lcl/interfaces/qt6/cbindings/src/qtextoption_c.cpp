//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextoption_c.h"

QTextOptionH QTextOption_Create()
{
	return (QTextOptionH) new QTextOption();
}

void QTextOption_Destroy(QTextOptionH handle)
{
	delete (QTextOption *)handle;
}

QTextOptionH QTextOption_Create2(unsigned int alignment)
{
	return (QTextOptionH) new QTextOption((Qt::Alignment)alignment);
}

QTextOptionH QTextOption_Create3(const QTextOptionH o)
{
	return (QTextOptionH) new QTextOption(*(const QTextOption*)o);
}

void QTextOption_setAlignment(QTextOptionH handle, Qt::Alignment alignment)
{
	((QTextOption *)handle)->setAlignment((Qt::Alignment)alignment);
}

Qt::Alignment QTextOption_alignment(QTextOptionH handle)
{
	return (Qt::Alignment) ((QTextOption *)handle)->alignment();
}

void QTextOption_setTextDirection(QTextOptionH handle, Qt::LayoutDirection aDirection)
{
	((QTextOption *)handle)->setTextDirection(aDirection);
}

Qt::LayoutDirection QTextOption_textDirection(QTextOptionH handle)
{
	return (Qt::LayoutDirection) ((QTextOption *)handle)->textDirection();
}

void QTextOption_setWrapMode(QTextOptionH handle, QTextOption::WrapMode wrap)
{
	((QTextOption *)handle)->setWrapMode(wrap);
}

QTextOption::WrapMode QTextOption_wrapMode(QTextOptionH handle)
{
	return (QTextOption::WrapMode) ((QTextOption *)handle)->wrapMode();
}

void QTextOption_setFlags(QTextOptionH handle, QTextOption::Flags flags)
{
	((QTextOption *)handle)->setFlags((QTextOption::Flags)flags);
}

QTextOption::Flags QTextOption_flags(QTextOptionH handle)
{
	return (QTextOption::Flags) ((QTextOption *)handle)->flags();
}

void QTextOption_setTabStopDistance(QTextOptionH handle, qreal tabStop)
{
	((QTextOption *)handle)->setTabStopDistance(tabStop);
}

qreal QTextOption_tabStopDistance(QTextOptionH handle)
{
	return (qreal) ((QTextOption *)handle)->tabStopDistance();
}

void QTextOption_setTabArray(QTextOptionH handle, PPtrIntArray tabStops)
{
	QList<qreal> t_tabStops;
	copyPtrIntArrayToQListTemplate(tabStops, t_tabStops);
	((QTextOption *)handle)->setTabArray(t_tabStops);
}

void QTextOption_tabArray(QTextOptionH handle, PPtrIntArray retval)
{
	QList<qreal> t_retval;
	t_retval = ((QTextOption *)handle)->tabArray();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QTextOption_setUseDesignMetrics(QTextOptionH handle, bool b)
{
	((QTextOption *)handle)->setUseDesignMetrics(b);
}

bool QTextOption_useDesignMetrics(QTextOptionH handle)
{
	return (bool) ((QTextOption *)handle)->useDesignMetrics();
}

