//******************************************************************************
//  Copyright (c) 2007 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSURFACEFORMAT_C_H
#define QSURFACEFORMAT_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QSurfaceFormatH QSurfaceFormat_Create();
C_EXPORT QSurfaceFormatH QSurfaceFormat_Create2(QSurfaceFormat::FormatOptions options);
C_EXPORT void QSurfaceFormat_Destroy(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setDepthBufferSize(QSurfaceFormatH handle,int size);
C_EXPORT int QSurfaceFormat_depthBufferSize(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setStencilBufferSize(QSurfaceFormatH handle, int size);
C_EXPORT int QSurfaceFormat_stencilBufferSize(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setRedBufferSize(QSurfaceFormatH handle, int size);
C_EXPORT int QSurfaceFormat_redBufferSize(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setGreenBufferSize(QSurfaceFormatH handle, int size);
C_EXPORT int QSurfaceFormat_greenBufferSize(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setBlueBufferSize(QSurfaceFormatH handle, int size);
C_EXPORT int QSurfaceFormat_blueBufferSize(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setAlphaBufferSize(QSurfaceFormatH handle, int size);
C_EXPORT int QSurfaceFormat_alphaBufferSize(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setSamples(QSurfaceFormatH handle, int numSamples);
C_EXPORT int QSurfaceFormat_samples(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setSwapBehavior(QSurfaceFormatH handle, QSurfaceFormat::SwapBehavior behavior);
C_EXPORT QSurfaceFormat::SwapBehavior QSurfaceFormat_swapBehavior(QSurfaceFormatH handle);
C_EXPORT bool QSurfaceFormat_hasAlpha(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setProfile(QSurfaceFormatH handle, QSurfaceFormat::OpenGLContextProfile profile);
C_EXPORT QSurfaceFormat::OpenGLContextProfile QSurfaceFormat_profile(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setRenderableType(QSurfaceFormatH handle, QSurfaceFormat::RenderableType type);
C_EXPORT QSurfaceFormat::RenderableType QSurfaceFormat_renderableType(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setMajorVersion(QSurfaceFormatH handle, int majorVersion);
C_EXPORT int QSurfaceFormat_majorVersion(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setMinorVersion(QSurfaceFormatH handle, int minorVersion);
C_EXPORT int QSurfaceFormat_minorVersion(QSurfaceFormatH handle);
C_EXPORT bool QSurfaceFormat_stereo(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setStereo(QSurfaceFormatH handle, bool enable);
C_EXPORT void QSurfaceFormat_setOptions(QSurfaceFormatH handle, QSurfaceFormat::FormatOptions options);
C_EXPORT void QSurfaceFormat_setOption(QSurfaceFormatH handle, QSurfaceFormat::FormatOption option, bool on = true);
C_EXPORT bool QSurfaceFormat_testOption(QSurfaceFormatH handle, QSurfaceFormat::FormatOption option);
C_EXPORT QSurfaceFormat::FormatOptions QSurfaceFormat_options(QSurfaceFormatH handle);
C_EXPORT int QSurfaceFormat_swapInterval(QSurfaceFormatH handle);
C_EXPORT void QSurfaceFormat_setSwapInterval(QSurfaceFormatH handle, int interval);
C_EXPORT void QSurfaceFormat_setDefaultFormat(QSurfaceFormatH format);
C_EXPORT void QSurfaceFormat_defaultFormat(QSurfaceFormatH retval);

#endif
