//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPICTURE_C_H
#define QPICTURE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QPictureH QPicture_Create(int formatVersion);
C_EXPORT void QPicture_Destroy(QPictureH handle);
C_EXPORT QPictureH QPicture_Create2(const QPictureH AnonParam1);
C_EXPORT bool QPicture_isNull(QPictureH handle);
C_EXPORT int QPicture_devType(QPictureH handle);
C_EXPORT uint QPicture_size(QPictureH handle);
C_EXPORT const char* QPicture_data(QPictureH handle);
C_EXPORT void QPicture_setData(QPictureH handle, const char* data, uint size);
C_EXPORT bool QPicture_play(QPictureH handle, QPainterH p);
C_EXPORT bool QPicture_load(QPictureH handle, QIODeviceH dev);
C_EXPORT bool QPicture_load2(QPictureH handle, PWideString fileName);
C_EXPORT bool QPicture_save(QPictureH handle, QIODeviceH dev);
C_EXPORT bool QPicture_save2(QPictureH handle, PWideString fileName);
C_EXPORT void QPicture_boundingRect(QPictureH handle, PRect retval);
C_EXPORT void QPicture_setBoundingRect(QPictureH handle, PRect r);
C_EXPORT void QPicture_swap(QPictureH handle, QPictureH other);
C_EXPORT void QPicture_detach(QPictureH handle);
C_EXPORT bool QPicture_isDetached(QPictureH handle);
C_EXPORT QPaintEngineH QPicture_paintEngine(QPictureH handle);

#endif
