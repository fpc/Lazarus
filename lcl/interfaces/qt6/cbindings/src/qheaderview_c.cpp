//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qheaderview_c.h"

QHeaderViewH QHeaderView_Create(Qt::Orientation orientation, QWidgetH parent)
{
	return (QHeaderViewH) new QHeaderView(orientation, (QWidget*)parent);
}

void QHeaderView_Destroy(QHeaderViewH handle)
{
	delete (QHeaderView *)handle;
}

void QHeaderView_setModel(QHeaderViewH handle, QAbstractItemModelH model)
{
	((QHeaderView *)handle)->setModel((QAbstractItemModel*)model);
}

Qt::Orientation QHeaderView_orientation(QHeaderViewH handle)
{
	return (Qt::Orientation) ((QHeaderView *)handle)->orientation();
}

int QHeaderView_offset(QHeaderViewH handle)
{
	return (int) ((QHeaderView *)handle)->offset();
}

int QHeaderView_length(QHeaderViewH handle)
{
	return (int) ((QHeaderView *)handle)->length();
}

void QHeaderView_sizeHint(QHeaderViewH handle, PSize retval)
{
	*(QSize *)retval = ((QHeaderView *)handle)->sizeHint();
}

int QHeaderView_sectionSizeHint(QHeaderViewH handle, int logicalIndex)
{
	return (int) ((QHeaderView *)handle)->sectionSizeHint(logicalIndex);
}

int QHeaderView_visualIndexAt(QHeaderViewH handle, int position)
{
	return (int) ((QHeaderView *)handle)->visualIndexAt(position);
}

int QHeaderView_logicalIndexAt(QHeaderViewH handle, int position)
{
	return (int) ((QHeaderView *)handle)->logicalIndexAt(position);
}

int QHeaderView_logicalIndexAt2(QHeaderViewH handle, int x, int y)
{
	return (int) ((QHeaderView *)handle)->logicalIndexAt(x, y);
}

int QHeaderView_logicalIndexAt3(QHeaderViewH handle, const QPointH pos)
{
	return (int) ((QHeaderView *)handle)->logicalIndexAt(*(const QPoint*)pos);
}

int QHeaderView_sectionSize(QHeaderViewH handle, int logicalIndex)
{
	return (int) ((QHeaderView *)handle)->sectionSize(logicalIndex);
}

int QHeaderView_sectionPosition(QHeaderViewH handle, int logicalIndex)
{
	return (int) ((QHeaderView *)handle)->sectionPosition(logicalIndex);
}

int QHeaderView_sectionViewportPosition(QHeaderViewH handle, int logicalIndex)
{
	return (int) ((QHeaderView *)handle)->sectionViewportPosition(logicalIndex);
}

void QHeaderView_moveSection(QHeaderViewH handle, int from, int to)
{
	((QHeaderView *)handle)->moveSection(from, to);
}

void QHeaderView_swapSections(QHeaderViewH handle, int first, int second)
{
	((QHeaderView *)handle)->swapSections(first, second);
}

void QHeaderView_resizeSection(QHeaderViewH handle, int logicalIndex, int size)
{
	((QHeaderView *)handle)->resizeSection(logicalIndex, size);
}

void QHeaderView_resizeSections(QHeaderViewH handle, QHeaderView::ResizeMode mode)
{
	((QHeaderView *)handle)->resizeSections(mode);
}

bool QHeaderView_isSectionHidden(QHeaderViewH handle, int logicalIndex)
{
	return (bool) ((QHeaderView *)handle)->isSectionHidden(logicalIndex);
}

void QHeaderView_setSectionHidden(QHeaderViewH handle, int logicalIndex, bool hide)
{
	((QHeaderView *)handle)->setSectionHidden(logicalIndex, hide);
}

int QHeaderView_hiddenSectionCount(QHeaderViewH handle)
{
	return (int) ((QHeaderView *)handle)->hiddenSectionCount();
}

void QHeaderView_hideSection(QHeaderViewH handle, int logicalIndex)
{
	((QHeaderView *)handle)->hideSection(logicalIndex);
}

void QHeaderView_showSection(QHeaderViewH handle, int logicalIndex)
{
	((QHeaderView *)handle)->showSection(logicalIndex);
}

int QHeaderView_count(QHeaderViewH handle)
{
	return (int) ((QHeaderView *)handle)->count();
}

int QHeaderView_visualIndex(QHeaderViewH handle, int logicalIndex)
{
	return (int) ((QHeaderView *)handle)->visualIndex(logicalIndex);
}

int QHeaderView_logicalIndex(QHeaderViewH handle, int visualIndex)
{
	return (int) ((QHeaderView *)handle)->logicalIndex(visualIndex);
}

void QHeaderView_setSectionsMovable(QHeaderViewH handle, bool movable)
{
	((QHeaderView *)handle)->setSectionsMovable(movable);
}

bool QHeaderView_sectionsMovable(QHeaderViewH handle)
{
	return (bool) ((QHeaderView *)handle)->sectionsMovable();
}

void QHeaderView_setSectionsClickable(QHeaderViewH handle, bool clickable)
{
	((QHeaderView *)handle)->setSectionsClickable(clickable);
}

bool QHeaderView_sectionsClickable(QHeaderViewH handle)
{
	return (bool) ((QHeaderView *)handle)->sectionsClickable();
}

void QHeaderView_setHighlightSections(QHeaderViewH handle, bool highlight)
{
	((QHeaderView *)handle)->setHighlightSections(highlight);
}

bool QHeaderView_highlightSections(QHeaderViewH handle)
{
	return (bool) ((QHeaderView *)handle)->highlightSections();
}

QHeaderView::ResizeMode QHeaderView_sectionResizeMode(QHeaderViewH handle, int logicalIndex)
{
	return (QHeaderView::ResizeMode) ((QHeaderView *)handle)->sectionResizeMode(logicalIndex);
}

void QHeaderView_setSectionResizeMode(QHeaderViewH handle, QHeaderView::ResizeMode mode)
{
	((QHeaderView *)handle)->setSectionResizeMode(mode);
}

void QHeaderView_setSectionResizeMode2(QHeaderViewH handle, int logicalIndex, QHeaderView::ResizeMode mode)
{
	((QHeaderView *)handle)->setSectionResizeMode(logicalIndex, mode);
}

int QHeaderView_stretchSectionCount(QHeaderViewH handle)
{
	return (int) ((QHeaderView *)handle)->stretchSectionCount();
}

void QHeaderView_setSortIndicatorShown(QHeaderViewH handle, bool show)
{
	((QHeaderView *)handle)->setSortIndicatorShown(show);
}

bool QHeaderView_isSortIndicatorShown(QHeaderViewH handle)
{
	return (bool) ((QHeaderView *)handle)->isSortIndicatorShown();
}

void QHeaderView_setSortIndicator(QHeaderViewH handle, int logicalIndex, Qt::SortOrder order)
{
	((QHeaderView *)handle)->setSortIndicator(logicalIndex, order);
}

int QHeaderView_sortIndicatorSection(QHeaderViewH handle)
{
	return (int) ((QHeaderView *)handle)->sortIndicatorSection();
}

Qt::SortOrder QHeaderView_sortIndicatorOrder(QHeaderViewH handle)
{
	return (Qt::SortOrder) ((QHeaderView *)handle)->sortIndicatorOrder();
}

bool QHeaderView_stretchLastSection(QHeaderViewH handle)
{
	return (bool) ((QHeaderView *)handle)->stretchLastSection();
}

void QHeaderView_setStretchLastSection(QHeaderViewH handle, bool stretch)
{
	((QHeaderView *)handle)->setStretchLastSection(stretch);
}

bool QHeaderView_cascadingSectionResizes(QHeaderViewH handle)
{
	return (bool) ((QHeaderView *)handle)->cascadingSectionResizes();
}

void QHeaderView_setCascadingSectionResizes(QHeaderViewH handle, bool enable)
{
	((QHeaderView *)handle)->setCascadingSectionResizes(enable);
}

int QHeaderView_defaultSectionSize(QHeaderViewH handle)
{
	return (int) ((QHeaderView *)handle)->defaultSectionSize();
}

void QHeaderView_setDefaultSectionSize(QHeaderViewH handle, int size)
{
	((QHeaderView *)handle)->setDefaultSectionSize(size);
}

int QHeaderView_minimumSectionSize(QHeaderViewH handle)
{
	return (int) ((QHeaderView *)handle)->minimumSectionSize();
}

void QHeaderView_setMinimumSectionSize(QHeaderViewH handle, int size)
{
	((QHeaderView *)handle)->setMinimumSectionSize(size);
}

unsigned int QHeaderView_defaultAlignment(QHeaderViewH handle)
{
	return (unsigned int) ((QHeaderView *)handle)->defaultAlignment();
}

void QHeaderView_setDefaultAlignment(QHeaderViewH handle, unsigned int alignment)
{
	((QHeaderView *)handle)->setDefaultAlignment((Qt::Alignment)alignment);
}

void QHeaderView_doItemsLayout(QHeaderViewH handle)
{
	((QHeaderView *)handle)->doItemsLayout();
}

bool QHeaderView_sectionsMoved(QHeaderViewH handle)
{
	return (bool) ((QHeaderView *)handle)->sectionsMoved();
}

bool QHeaderView_sectionsHidden(QHeaderViewH handle)
{
	return (bool) ((QHeaderView *)handle)->sectionsHidden();
}

void QHeaderView_saveState(QHeaderViewH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QHeaderView *)handle)->saveState();
}

bool QHeaderView_restoreState(QHeaderViewH handle, const QByteArrayH state)
{
	return (bool) ((QHeaderView *)handle)->restoreState(*(const QByteArray*)state);
}

void QHeaderView_reset(QHeaderViewH handle)
{
	((QHeaderView *)handle)->reset();
}

void QHeaderView_setOffset(QHeaderViewH handle, int offset)
{
	((QHeaderView *)handle)->setOffset(offset);
}

void QHeaderView_setOffsetToSectionPosition(QHeaderViewH handle, int visualIndex)
{
	((QHeaderView *)handle)->setOffsetToSectionPosition(visualIndex);
}

void QHeaderView_setOffsetToLastSection(QHeaderViewH handle)
{
	((QHeaderView *)handle)->setOffsetToLastSection();
}

void QHeaderView_headerDataChanged(QHeaderViewH handle, Qt::Orientation orientation, int logicalFirst, int logicalLast)
{
	((QHeaderView *)handle)->headerDataChanged(orientation, logicalFirst, logicalLast);
}

