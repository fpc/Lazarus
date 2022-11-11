//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qframe_c.h"

QFrameH QFrame_Create(QWidgetH parent, unsigned int f)
{
	return (QFrameH) new QFrame((QWidget*)parent, (Qt::WindowFlags)f);
}

void QFrame_Destroy(QFrameH handle)
{
	delete (QFrame *)handle;
}

int QFrame_frameStyle(QFrameH handle)
{
	return (int) ((QFrame *)handle)->frameStyle();
}

void QFrame_setFrameStyle(QFrameH handle, int AnonParam1)
{
	((QFrame *)handle)->setFrameStyle(AnonParam1);
}

int QFrame_frameWidth(QFrameH handle)
{
	return (int) ((QFrame *)handle)->frameWidth();
}

void QFrame_sizeHint(QFrameH handle, PSize retval)
{
	*(QSize *)retval = ((QFrame *)handle)->sizeHint();
}

QFrame::Shape QFrame_frameShape(QFrameH handle)
{
	return (QFrame::Shape) ((QFrame *)handle)->frameShape();
}

void QFrame_setFrameShape(QFrameH handle, QFrame::Shape AnonParam1)
{
	((QFrame *)handle)->setFrameShape(AnonParam1);
}

QFrame::Shadow QFrame_frameShadow(QFrameH handle)
{
	return (QFrame::Shadow) ((QFrame *)handle)->frameShadow();
}

void QFrame_setFrameShadow(QFrameH handle, QFrame::Shadow AnonParam1)
{
	((QFrame *)handle)->setFrameShadow(AnonParam1);
}

int QFrame_lineWidth(QFrameH handle)
{
	return (int) ((QFrame *)handle)->lineWidth();
}

void QFrame_setLineWidth(QFrameH handle, int AnonParam1)
{
	((QFrame *)handle)->setLineWidth(AnonParam1);
}

int QFrame_midLineWidth(QFrameH handle)
{
	return (int) ((QFrame *)handle)->midLineWidth();
}

void QFrame_setMidLineWidth(QFrameH handle, int AnonParam1)
{
	((QFrame *)handle)->setMidLineWidth(AnonParam1);
}

void QFrame_frameRect(QFrameH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QFrame *)handle)->frameRect();
	copyQRectToPRect(t_retval, retval);
}

void QFrame_setFrameRect(QFrameH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QFrame *)handle)->setFrameRect(t_AnonParam1);
}

