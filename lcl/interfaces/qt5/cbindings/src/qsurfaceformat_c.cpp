//******************************************************************************
//  Copyright (c) 2017 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsurfaceformat_c.h"

QSurfaceFormatH QSurfaceFormat_Create()
{
  return (QSurfaceFormatH) new QSurfaceFormat();
}

QSurfaceFormatH QSurfaceFormat_Create2(QSurfaceFormat::FormatOptions options)
{
  return (QSurfaceFormatH) new QSurfaceFormat((QSurfaceFormat::FormatOptions) options);
}

void QSurfaceFormat_Destroy(QSurfaceFormatH handle)
{
	delete (QSurfaceFormat *)handle;
}

void QSurfaceFormat_setDepthBufferSize(QSurfaceFormatH handle, int size)
{
  ((QSurfaceFormat*)handle)->setDepthBufferSize(size);
}

int QSurfaceFormat_depthBufferSize(QSurfaceFormatH handle)
{
  return (int) ((QSurfaceFormat*)handle)->depthBufferSize();
}

void QSurfaceFormat_setStencilBufferSize(QSurfaceFormatH handle, int size)
{
  ((QSurfaceFormat*)handle)->setStencilBufferSize(size);
}

int QSurfaceFormat_stencilBufferSize(QSurfaceFormatH handle)
{
  return (int) ((QSurfaceFormat*)handle)->stencilBufferSize();
}

void QSurfaceFormat_setRedBufferSize(QSurfaceFormatH handle, int size)
{
  ((QSurfaceFormat*)handle)->setRedBufferSize(size);
}

int QSurfaceFormat_redBufferSize(QSurfaceFormatH handle)
{
  return (int) ((QSurfaceFormat*)handle)->redBufferSize();
}

void QSurfaceFormat_setGreenBufferSize(QSurfaceFormatH handle, int size)
{
  ((QSurfaceFormat*)handle)->setGreenBufferSize(size);
}

int QSurfaceFormat_greenBufferSize(QSurfaceFormatH handle)
{
  return (int) ((QSurfaceFormat*)handle)->greenBufferSize();
}

void QSurfaceFormat_setBlueBufferSize(QSurfaceFormatH handle, int size)
{
  ((QSurfaceFormat*)handle)->setBlueBufferSize(size);
}

int QSurfaceFormat_blueBufferSize(QSurfaceFormatH handle)
{
  return (int) ((QSurfaceFormat*)handle)->blueBufferSize();
}

void QSurfaceFormat_setAlphaBufferSize(QSurfaceFormatH handle, int size)
{
  ((QSurfaceFormat*)handle)->setAlphaBufferSize(size);
}

int QSurfaceFormat_alphaBufferSize(QSurfaceFormatH handle)
{
  return (int) ((QSurfaceFormat*)handle)->alphaBufferSize();
}

void QSurfaceFormat_setSamples(QSurfaceFormatH handle, int numSamples)
{
  ((QSurfaceFormat*)handle)->setSamples(numSamples);
}

int QSurfaceFormat_samples(QSurfaceFormatH handle)
{
  return (int) ((QSurfaceFormat*)handle)->samples();
}

void QSurfaceFormat_setSwapBehavior(QSurfaceFormatH handle, QSurfaceFormat::SwapBehavior behavior)
{
  ((QSurfaceFormat*)handle)->setSwapBehavior((QSurfaceFormat::SwapBehavior) behavior);
}

QSurfaceFormat::SwapBehavior QSurfaceFormat_swapBehavior(QSurfaceFormatH handle)
{
  return (QSurfaceFormat::SwapBehavior) ((QSurfaceFormat*)handle)->swapBehavior();
}

bool QSurfaceFormat_hasAlpha(QSurfaceFormatH handle)
{
  return (bool) ((QSurfaceFormat*)handle)->hasAlpha();
}

void QSurfaceFormat_setProfile(QSurfaceFormatH handle, QSurfaceFormat::OpenGLContextProfile profile)
{
  ((QSurfaceFormat*)handle)->setProfile((QSurfaceFormat::OpenGLContextProfile) profile);
}

QSurfaceFormat::OpenGLContextProfile QSurfaceFormat_profile(QSurfaceFormatH handle)
{
  return (QSurfaceFormat::OpenGLContextProfile) ((QSurfaceFormat*)handle)->profile();
}

void QSurfaceFormat_setRenderableType(QSurfaceFormatH handle, QSurfaceFormat::RenderableType type)
{
  ((QSurfaceFormat*)handle)->setRenderableType((QSurfaceFormat::RenderableType) type);
}

QSurfaceFormat::RenderableType QSurfaceFormat_renderableType(QSurfaceFormatH handle)
{
  return (QSurfaceFormat::RenderableType) ((QSurfaceFormat*)handle)->renderableType();
}

void QSurfaceFormat_setMajorVersion(QSurfaceFormatH handle, int majorVersion)
{
  ((QSurfaceFormat*)handle)->setMajorVersion(majorVersion);
}

int QSurfaceFormat_majorVersion(QSurfaceFormatH handle)
{
  return (int) ((QSurfaceFormat*)handle)->majorVersion();
}

void QSurfaceFormat_setMinorVersion(QSurfaceFormatH handle, int minorVersion)
{
  ((QSurfaceFormat*)handle)->setMinorVersion(minorVersion);
}

int QSurfaceFormat_minorVersion(QSurfaceFormatH handle)
{
  return (int) ((QSurfaceFormat*)handle)->minorVersion();
}

bool QSurfaceFormat_stereo(QSurfaceFormatH handle)
{
  return (bool) ((QSurfaceFormat*)handle)->stereo();
}

void QSurfaceFormat_setStereo(QSurfaceFormatH handle, bool enable)
{
  ((QSurfaceFormat*)handle)->setStereo(enable);
}

void QSurfaceFormat_setOptions(QSurfaceFormatH handle, QSurfaceFormat::FormatOptions options)
{
  ((QSurfaceFormat*)handle)->setOptions((QSurfaceFormat::FormatOptions) options);
}

void QSurfaceFormat_setOption(QSurfaceFormatH handle, QSurfaceFormat::FormatOption option, bool on)
{
  ((QSurfaceFormat*)handle)->setOption((QSurfaceFormat::FormatOption) option, on);
}

bool QSurfaceFormat_testOption(QSurfaceFormatH handle, QSurfaceFormat::FormatOption option)
{
  return (bool) ((QSurfaceFormat*)handle)->testOption((QSurfaceFormat::FormatOption) option);
}

QSurfaceFormat::FormatOptions QSurfaceFormat_options(QSurfaceFormatH handle)
{
  return (QSurfaceFormat::FormatOptions) ((QSurfaceFormat*)handle)->options();
}

int QSurfaceFormat_swapInterval(QSurfaceFormatH handle)
{
  return ((QSurfaceFormat*)handle)->swapInterval();
}

void QSurfaceFormat_setSwapInterval(QSurfaceFormatH handle, int interval)
{
  ((QSurfaceFormat*)handle)->setSwapInterval(interval);
}

void QSurfaceFormat_setDefaultFormat(QSurfaceFormatH format)
{
  QSurfaceFormat::setDefaultFormat(*(const QSurfaceFormat*) format);
}

void QSurfaceFormat_defaultFormat(QSurfaceFormatH retval)
{
  *(QSurfaceFormat*)retval = QSurfaceFormat::defaultFormat();
}
