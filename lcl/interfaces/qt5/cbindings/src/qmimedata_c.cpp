//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmimedata_c.h"

QMimeDataH QMimeData_Create()
{
	return (QMimeDataH) new QMimeData();
}

void QMimeData_Destroy(QMimeDataH handle)
{
	delete (QMimeData *)handle;
}

bool QMimeData_hasUrls(QMimeDataH handle)
{
	return (bool) ((QMimeData *)handle)->hasUrls();
}

void QMimeData_text(QMimeDataH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QMimeData *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QMimeData_setText(QMimeDataH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QMimeData *)handle)->setText(t_text);
}

bool QMimeData_hasText(QMimeDataH handle)
{
	return (bool) ((QMimeData *)handle)->hasText();
}

void QMimeData_html(QMimeDataH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QMimeData *)handle)->html();
	copyQStringToPWideString(t_retval, retval);
}

void QMimeData_setHtml(QMimeDataH handle, PWideString html)
{
	QString t_html;
	copyPWideStringToQString(html, t_html);
	((QMimeData *)handle)->setHtml(t_html);
}

bool QMimeData_hasHtml(QMimeDataH handle)
{
	return (bool) ((QMimeData *)handle)->hasHtml();
}

void QMimeData_imageData(QMimeDataH handle, QVariantH retval)
{
	*(QVariant *)retval = ((QMimeData *)handle)->imageData();
}

void QMimeData_setImageData(QMimeDataH handle, const QVariantH image)
{
	((QMimeData *)handle)->setImageData(*(const QVariant*)image);
}

bool QMimeData_hasImage(QMimeDataH handle)
{
	return (bool) ((QMimeData *)handle)->hasImage();
}

void QMimeData_colorData(QMimeDataH handle, QVariantH retval)
{
	*(QVariant *)retval = ((QMimeData *)handle)->colorData();
}

void QMimeData_setColorData(QMimeDataH handle, const QVariantH color)
{
	((QMimeData *)handle)->setColorData(*(const QVariant*)color);
}

bool QMimeData_hasColor(QMimeDataH handle)
{
	return (bool) ((QMimeData *)handle)->hasColor();
}

void QMimeData_data(QMimeDataH handle, QByteArrayH retval, PWideString mimetype)
{
	QString t_mimetype;
	copyPWideStringToQString(mimetype, t_mimetype);
	*(QByteArray *)retval = ((QMimeData *)handle)->data(t_mimetype);
}

void QMimeData_setData(QMimeDataH handle, PWideString mimetype, const QByteArrayH data)
{
	QString t_mimetype;
	copyPWideStringToQString(mimetype, t_mimetype);
	((QMimeData *)handle)->setData(t_mimetype, *(const QByteArray*)data);
}

void QMimeData_removeFormat(QMimeDataH handle, PWideString mimetype)
{
	QString t_mimetype;
	copyPWideStringToQString(mimetype, t_mimetype);
	((QMimeData *)handle)->removeFormat(t_mimetype);
}

bool QMimeData_hasFormat(QMimeDataH handle, PWideString mimetype)
{
	QString t_mimetype;
	copyPWideStringToQString(mimetype, t_mimetype);
	return (bool) ((QMimeData *)handle)->hasFormat(t_mimetype);
}

void QMimeData_formats(QMimeDataH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QMimeData *)handle)->formats();
}

void QMimeData_clear(QMimeDataH handle)
{
	((QMimeData *)handle)->clear();
}

