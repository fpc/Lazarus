//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qdialog_c.h"

QDialogH QDialog_Create(QWidgetH parent, unsigned int f)
{
	return (QDialogH) new QDialog((QWidget*)parent, (Qt::WindowFlags)f);
}

void QDialog_Destroy(QDialogH handle)
{
	delete (QDialog *)handle;
}

int QDialog_result(QDialogH handle)
{
	return (int) ((QDialog *)handle)->result();
}

void QDialog_setVisible(QDialogH handle, bool visible)
{
	((QDialog *)handle)->setVisible(visible);
}

void QDialog_setOrientation(QDialogH handle, Qt::Orientation orientation)
{
	((QDialog *)handle)->setOrientation(orientation);
}

Qt::Orientation QDialog_orientation(QDialogH handle)
{
	return (Qt::Orientation) ((QDialog *)handle)->orientation();
}

void QDialog_setExtension(QDialogH handle, QWidgetH extension)
{
	((QDialog *)handle)->setExtension((QWidget*)extension);
}

QWidgetH QDialog_extension(QDialogH handle)
{
	return (QWidgetH) ((QDialog *)handle)->extension();
}

void QDialog_sizeHint(QDialogH handle, PSize retval)
{
	*(QSize *)retval = ((QDialog *)handle)->sizeHint();
}

void QDialog_minimumSizeHint(QDialogH handle, PSize retval)
{
	*(QSize *)retval = ((QDialog *)handle)->minimumSizeHint();
}

void QDialog_setSizeGripEnabled(QDialogH handle, bool AnonParam1)
{
	((QDialog *)handle)->setSizeGripEnabled(AnonParam1);
}

bool QDialog_isSizeGripEnabled(QDialogH handle)
{
	return (bool) ((QDialog *)handle)->isSizeGripEnabled();
}

void QDialog_setModal(QDialogH handle, bool modal)
{
	((QDialog *)handle)->setModal(modal);
}

void QDialog_setResult(QDialogH handle, int r)
{
	((QDialog *)handle)->setResult(r);
}

void QDialog_open(QDialogH handle)
{
	((QDialog *)handle)->open();
}

int QDialog_exec(QDialogH handle)
{
	return (int) ((QDialog *)handle)->exec();
}

void QDialog_done(QDialogH handle, int AnonParam1)
{
	((QDialog *)handle)->done(AnonParam1);
}

void QDialog_accept(QDialogH handle)
{
	((QDialog *)handle)->accept();
}

void QDialog_reject(QDialogH handle)
{
	((QDialog *)handle)->reject();
}

void QDialog_showExtension(QDialogH handle, bool AnonParam1)
{
	((QDialog *)handle)->showExtension(AnonParam1);
}

