//******************************************************************************
//  Copyright (c) 2017-2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsurface_c.h"

void QSurface_Destroy(QSurfaceH handle)
{
	delete (QSurface *)handle;
}

void QSurface_size(QSurfaceH handle, PSize retval)
{
	*(QSize *)retval = ((QSurface *)handle)->size();
}


bool QSurface_supportsOpenGL(QSurfaceH handle)
{
  return (bool) ((QSurface *)handle)->supportsOpenGL();
}

QSurface::SurfaceClass QSurface_surfaceClass(QSurfaceH handle)
{
  return (QSurface::SurfaceClass) ((QSurface *)handle)->surfaceClass();
}

QSurface::SurfaceType QSurface_surfaceType(QSurfaceH handle)
{
  return (QSurface::SurfaceType) ((QSurface *)handle)->surfaceType();
}


void QSurface_format(QSurfaceH handle, QSurfaceFormatH areturn)
{
  *(QSurfaceFormat *)areturn = ((QSurface *)handle)->format();
}

