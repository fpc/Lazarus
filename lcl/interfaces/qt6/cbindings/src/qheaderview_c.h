//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QHEADERVIEW_C_H
#define QHEADERVIEW_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QHeaderViewH QHeaderView_Create(Qt::Orientation orientation, QWidgetH parent);
C_EXPORT void QHeaderView_Destroy(QHeaderViewH handle);
C_EXPORT void QHeaderView_setModel(QHeaderViewH handle, QAbstractItemModelH model);
C_EXPORT Qt::Orientation QHeaderView_orientation(QHeaderViewH handle);
C_EXPORT int QHeaderView_offset(QHeaderViewH handle);
C_EXPORT int QHeaderView_length(QHeaderViewH handle);
C_EXPORT void QHeaderView_sizeHint(QHeaderViewH handle, PSize retval);
C_EXPORT int QHeaderView_sectionSizeHint(QHeaderViewH handle, int logicalIndex);
C_EXPORT int QHeaderView_visualIndexAt(QHeaderViewH handle, int position);
C_EXPORT int QHeaderView_logicalIndexAt(QHeaderViewH handle, int position);
C_EXPORT int QHeaderView_logicalIndexAt2(QHeaderViewH handle, int x, int y);
C_EXPORT int QHeaderView_logicalIndexAt3(QHeaderViewH handle, const QPointH pos);
C_EXPORT int QHeaderView_sectionSize(QHeaderViewH handle, int logicalIndex);
C_EXPORT int QHeaderView_sectionPosition(QHeaderViewH handle, int logicalIndex);
C_EXPORT int QHeaderView_sectionViewportPosition(QHeaderViewH handle, int logicalIndex);
C_EXPORT void QHeaderView_moveSection(QHeaderViewH handle, int from, int to);
C_EXPORT void QHeaderView_swapSections(QHeaderViewH handle, int first, int second);
C_EXPORT void QHeaderView_resizeSection(QHeaderViewH handle, int logicalIndex, int size);
C_EXPORT void QHeaderView_resizeSections(QHeaderViewH handle, QHeaderView::ResizeMode mode);
C_EXPORT bool QHeaderView_isSectionHidden(QHeaderViewH handle, int logicalIndex);
C_EXPORT void QHeaderView_setSectionHidden(QHeaderViewH handle, int logicalIndex, bool hide);
C_EXPORT int QHeaderView_hiddenSectionCount(QHeaderViewH handle);
C_EXPORT void QHeaderView_hideSection(QHeaderViewH handle, int logicalIndex);
C_EXPORT void QHeaderView_showSection(QHeaderViewH handle, int logicalIndex);
C_EXPORT int QHeaderView_count(QHeaderViewH handle);
C_EXPORT int QHeaderView_visualIndex(QHeaderViewH handle, int logicalIndex);
C_EXPORT int QHeaderView_logicalIndex(QHeaderViewH handle, int visualIndex);
C_EXPORT void QHeaderView_setSectionsMovable(QHeaderViewH handle, bool movable);
C_EXPORT bool QHeaderView_sectionsMovable(QHeaderViewH handle);
C_EXPORT void QHeaderView_setSectionsClickable(QHeaderViewH handle, bool clickable);
C_EXPORT bool QHeaderView_sectionsClickable(QHeaderViewH handle);
C_EXPORT void QHeaderView_setHighlightSections(QHeaderViewH handle, bool highlight);
C_EXPORT bool QHeaderView_highlightSections(QHeaderViewH handle);
C_EXPORT QHeaderView::ResizeMode QHeaderView_sectionResizeMode(QHeaderViewH handle, int logicalIndex);
C_EXPORT void QHeaderView_setSectionResizeMode(QHeaderViewH handle, QHeaderView::ResizeMode mode);
C_EXPORT void QHeaderView_setSectionResizeMode2(QHeaderViewH handle, int logicalIndex, QHeaderView::ResizeMode mode);
C_EXPORT int QHeaderView_stretchSectionCount(QHeaderViewH handle);
C_EXPORT void QHeaderView_setSortIndicatorShown(QHeaderViewH handle, bool show);
C_EXPORT bool QHeaderView_isSortIndicatorShown(QHeaderViewH handle);
C_EXPORT void QHeaderView_setSortIndicator(QHeaderViewH handle, int logicalIndex, Qt::SortOrder order);
C_EXPORT int QHeaderView_sortIndicatorSection(QHeaderViewH handle);
C_EXPORT Qt::SortOrder QHeaderView_sortIndicatorOrder(QHeaderViewH handle);
C_EXPORT bool QHeaderView_stretchLastSection(QHeaderViewH handle);
C_EXPORT void QHeaderView_setStretchLastSection(QHeaderViewH handle, bool stretch);
C_EXPORT bool QHeaderView_cascadingSectionResizes(QHeaderViewH handle);
C_EXPORT void QHeaderView_setCascadingSectionResizes(QHeaderViewH handle, bool enable);
C_EXPORT int QHeaderView_defaultSectionSize(QHeaderViewH handle);
C_EXPORT void QHeaderView_setDefaultSectionSize(QHeaderViewH handle, int size);
C_EXPORT int QHeaderView_minimumSectionSize(QHeaderViewH handle);
C_EXPORT void QHeaderView_setMinimumSectionSize(QHeaderViewH handle, int size);
C_EXPORT unsigned int QHeaderView_defaultAlignment(QHeaderViewH handle);
C_EXPORT void QHeaderView_setDefaultAlignment(QHeaderViewH handle, unsigned int alignment);
C_EXPORT void QHeaderView_doItemsLayout(QHeaderViewH handle);
C_EXPORT bool QHeaderView_sectionsMoved(QHeaderViewH handle);
C_EXPORT bool QHeaderView_sectionsHidden(QHeaderViewH handle);
C_EXPORT void QHeaderView_saveState(QHeaderViewH handle, QByteArrayH retval);
C_EXPORT bool QHeaderView_restoreState(QHeaderViewH handle, const QByteArrayH state);
C_EXPORT void QHeaderView_reset(QHeaderViewH handle);
C_EXPORT void QHeaderView_setOffset(QHeaderViewH handle, int offset);
C_EXPORT void QHeaderView_setOffsetToSectionPosition(QHeaderViewH handle, int visualIndex);
C_EXPORT void QHeaderView_setOffsetToLastSection(QHeaderViewH handle);
C_EXPORT void QHeaderView_headerDataChanged(QHeaderViewH handle, Qt::Orientation orientation, int logicalFirst, int logicalLast);

#endif
