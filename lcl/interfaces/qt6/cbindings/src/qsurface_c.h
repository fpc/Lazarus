//******************************************************************************
//  Copyright (c) 2017-2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSURFACE_C_H
#define QSURFACE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QSurface_Destroy(QSurfaceH handle);
C_EXPORT void QSurface_size(QSurfaceH handle, PSize retval);
C_EXPORT bool QSurface_supportsOpenGL(QSurfaceH handle);
C_EXPORT QSurface::SurfaceClass QSurface_surfaceClass(QSurfaceH handle);
C_EXPORT QSurface::SurfaceType QSurface_surfaceType(QSurfaceH handle);
C_EXPORT void QSurface_format(QSurfaceH handle, QSurfaceFormatH areturn);

#endif
