//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstyleoption_c.h"

int QStyleOption_version(QStyleOptionH handle)
{
	return (int) ((QStyleOption *)handle)->version;
}

void QStyleOption_setVersion(QStyleOptionH handle, int version)
{
	((QStyleOption *)handle)->version = version;
}

int QStyleOption_type(QStyleOptionH handle)
{
	return (int) ((QStyleOption *)handle)->type;
}

void QStyleOption_setType(QStyleOptionH handle, int type)
{
	((QStyleOption *)handle)->type = type;
}

unsigned int QStyleOption_state(QStyleOptionH handle)
{
	return (unsigned int) ((QStyleOption *)handle)->state;
}

void QStyleOption_setState(QStyleOptionH handle, unsigned int state)
{
	((QStyleOption *)handle)->state =(QStyle::State)state;
}

Qt::LayoutDirection QStyleOption_direction(QStyleOptionH handle)
{
	return (Qt::LayoutDirection) ((QStyleOption *)handle)->direction;
}

void QStyleOption_setDirection(QStyleOptionH handle, Qt::LayoutDirection direction)
{
	((QStyleOption *)handle)->direction = direction;
}

void QStyleOption_rect(QStyleOptionH handle, PRect retval)
{
	copyQRectToPRect(((QStyleOption *)handle)->rect,retval);
}

void QStyleOption_setRect(QStyleOptionH handle, PRect rect)
{
	copyPRectToQRect(rect,((QStyleOption *)handle)->rect);
}

void QStyleOption_fontMetrics(QStyleOptionH handle, QFontMetricsH retval)
{
	 *(QFontMetrics *)retval = ((QStyleOption *)handle)->fontMetrics;
}

void QStyleOption_setFontMetrics(QStyleOptionH handle, QFontMetricsH fontMetrics)
{
	((QStyleOption *)handle)->fontMetrics = *(QFontMetrics *)fontMetrics;
}

void QStyleOption_palette(QStyleOptionH handle, QPaletteH retval)
{
	 *(QPalette *)retval = ((QStyleOption *)handle)->palette;
}

void QStyleOption_setPalette(QStyleOptionH handle, QPaletteH palette)
{
	((QStyleOption *)handle)->palette = *(QPalette *)palette;
}

QObjectH QStyleOption_styleObject(QStyleOptionH handle)
{
	return (QObjectH) ((QStyleOption *)handle)->styleObject;
}

void QStyleOption_setStyleObject(QStyleOptionH handle, QObjectH styleObject)
{
	((QStyleOption *)handle)->styleObject =(QObject *)styleObject;
}

QStyleOptionH QStyleOption_Create(int version, int type)
{
	return (QStyleOptionH) new QStyleOption(version, type);
}

void QStyleOption_Destroy(QStyleOptionH handle)
{
	delete (QStyleOption *)handle;
}

QStyleOptionH QStyleOption_Create2(const QStyleOptionH other)
{
	return (QStyleOptionH) new QStyleOption(*(const QStyleOption*)other);
}

void QStyleOption_initFrom(QStyleOptionH handle, const QWidgetH w)
{
	((QStyleOption *)handle)->initFrom((const QWidget*)w);
}

void QStyleOptionFocusRect_backgroundColor(QStyleOptionFocusRectH handle, PQColor retval)
{
	 *(QColor *)retval = ((QStyleOptionFocusRect *)handle)->backgroundColor;
}

void QStyleOptionFocusRect_setBackgroundColor(QStyleOptionFocusRectH handle, PQColor backgroundColor)
{
	((QStyleOptionFocusRect *)handle)->backgroundColor = *(QColor *)backgroundColor;
}

QStyleOptionFocusRectH QStyleOptionFocusRect_Create()
{
	return (QStyleOptionFocusRectH) new QStyleOptionFocusRect();
}

void QStyleOptionFocusRect_Destroy(QStyleOptionFocusRectH handle)
{
	delete (QStyleOptionFocusRect *)handle;
}

QStyleOptionFocusRectH QStyleOptionFocusRect_Create2(const QStyleOptionFocusRectH other)
{
	return (QStyleOptionFocusRectH) new QStyleOptionFocusRect(*(const QStyleOptionFocusRect*)other);
}

int QStyleOptionFrame_lineWidth(QStyleOptionFrameH handle)
{
	return (int) ((QStyleOptionFrame *)handle)->lineWidth;
}

void QStyleOptionFrame_setLineWidth(QStyleOptionFrameH handle, int lineWidth)
{
	((QStyleOptionFrame *)handle)->lineWidth = lineWidth;
}

int QStyleOptionFrame_midLineWidth(QStyleOptionFrameH handle)
{
	return (int) ((QStyleOptionFrame *)handle)->midLineWidth;
}

void QStyleOptionFrame_setMidLineWidth(QStyleOptionFrameH handle, int midLineWidth)
{
	((QStyleOptionFrame *)handle)->midLineWidth = midLineWidth;
}

QStyleOptionFrameH QStyleOptionFrame_Create()
{
	return (QStyleOptionFrameH) new QStyleOptionFrame();
}

void QStyleOptionFrame_Destroy(QStyleOptionFrameH handle)
{
	delete (QStyleOptionFrame *)handle;
}

QStyleOptionFrameH QStyleOptionFrame_Create2(const QStyleOptionFrameH other)
{
	return (QStyleOptionFrameH) new QStyleOptionFrame(*(const QStyleOptionFrame*)other);
}

int QStyleOptionTabWidgetFrame_lineWidth(QStyleOptionTabWidgetFrameH handle)
{
	return (int) ((QStyleOptionTabWidgetFrame *)handle)->lineWidth;
}

void QStyleOptionTabWidgetFrame_setLineWidth(QStyleOptionTabWidgetFrameH handle, int lineWidth)
{
	((QStyleOptionTabWidgetFrame *)handle)->lineWidth = lineWidth;
}

int QStyleOptionTabWidgetFrame_midLineWidth(QStyleOptionTabWidgetFrameH handle)
{
	return (int) ((QStyleOptionTabWidgetFrame *)handle)->midLineWidth;
}

void QStyleOptionTabWidgetFrame_setMidLineWidth(QStyleOptionTabWidgetFrameH handle, int midLineWidth)
{
	((QStyleOptionTabWidgetFrame *)handle)->midLineWidth = midLineWidth;
}

QTabBar::Shape QStyleOptionTabWidgetFrame_shape(QStyleOptionTabWidgetFrameH handle)
{
	return (QTabBar::Shape) ((QStyleOptionTabWidgetFrame *)handle)->shape;
}

void QStyleOptionTabWidgetFrame_setShape(QStyleOptionTabWidgetFrameH handle, QTabBar::Shape shape)
{
	((QStyleOptionTabWidgetFrame *)handle)->shape = shape;
}

void QStyleOptionTabWidgetFrame_tabBarSize(QStyleOptionTabWidgetFrameH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionTabWidgetFrame *)handle)->tabBarSize;
}

void QStyleOptionTabWidgetFrame_setTabBarSize(QStyleOptionTabWidgetFrameH handle, PSize tabBarSize)
{
	((QStyleOptionTabWidgetFrame *)handle)->tabBarSize = *(QSize *)tabBarSize;
}

void QStyleOptionTabWidgetFrame_rightCornerWidgetSize(QStyleOptionTabWidgetFrameH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionTabWidgetFrame *)handle)->rightCornerWidgetSize;
}

void QStyleOptionTabWidgetFrame_setRightCornerWidgetSize(QStyleOptionTabWidgetFrameH handle, PSize rightCornerWidgetSize)
{
	((QStyleOptionTabWidgetFrame *)handle)->rightCornerWidgetSize = *(QSize *)rightCornerWidgetSize;
}

void QStyleOptionTabWidgetFrame_leftCornerWidgetSize(QStyleOptionTabWidgetFrameH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionTabWidgetFrame *)handle)->leftCornerWidgetSize;
}

void QStyleOptionTabWidgetFrame_setLeftCornerWidgetSize(QStyleOptionTabWidgetFrameH handle, PSize leftCornerWidgetSize)
{
	((QStyleOptionTabWidgetFrame *)handle)->leftCornerWidgetSize = *(QSize *)leftCornerWidgetSize;
}

void QStyleOptionTabWidgetFrame_tabBarRect(QStyleOptionTabWidgetFrameH handle, PRect retval)
{
	copyQRectToPRect(((QStyleOptionTabWidgetFrame *)handle)->tabBarRect,retval);
}

void QStyleOptionTabWidgetFrame_setTabBarRect(QStyleOptionTabWidgetFrameH handle, PRect tabBarRect)
{
	copyPRectToQRect(tabBarRect,((QStyleOptionTabWidgetFrame *)handle)->tabBarRect);
}

void QStyleOptionTabWidgetFrame_selectedTabRect(QStyleOptionTabWidgetFrameH handle, PRect retval)
{
	copyQRectToPRect(((QStyleOptionTabWidgetFrame *)handle)->selectedTabRect,retval);
}

void QStyleOptionTabWidgetFrame_setSelectedTabRect(QStyleOptionTabWidgetFrameH handle, PRect selectedTabRect)
{
	copyPRectToQRect(selectedTabRect,((QStyleOptionTabWidgetFrame *)handle)->selectedTabRect);
}

QStyleOptionTabWidgetFrameH QStyleOptionTabWidgetFrame_Create()
{
	return (QStyleOptionTabWidgetFrameH) new QStyleOptionTabWidgetFrame();
}

void QStyleOptionTabWidgetFrame_Destroy(QStyleOptionTabWidgetFrameH handle)
{
	delete (QStyleOptionTabWidgetFrame *)handle;
}

QStyleOptionTabWidgetFrameH QStyleOptionTabWidgetFrame_Create2(const QStyleOptionTabWidgetFrameH other)
{
	return (QStyleOptionTabWidgetFrameH) new QStyleOptionTabWidgetFrame(*(const QStyleOptionTabWidgetFrame*)other);
}

QTabBar::Shape QStyleOptionTabBarBase_shape(QStyleOptionTabBarBaseH handle)
{
	return (QTabBar::Shape) ((QStyleOptionTabBarBase *)handle)->shape;
}

void QStyleOptionTabBarBase_setShape(QStyleOptionTabBarBaseH handle, QTabBar::Shape shape)
{
	((QStyleOptionTabBarBase *)handle)->shape = shape;
}

void QStyleOptionTabBarBase_tabBarRect(QStyleOptionTabBarBaseH handle, PRect retval)
{
	copyQRectToPRect(((QStyleOptionTabBarBase *)handle)->tabBarRect,retval);
}

void QStyleOptionTabBarBase_setTabBarRect(QStyleOptionTabBarBaseH handle, PRect tabBarRect)
{
	copyPRectToQRect(tabBarRect,((QStyleOptionTabBarBase *)handle)->tabBarRect);
}

void QStyleOptionTabBarBase_selectedTabRect(QStyleOptionTabBarBaseH handle, PRect retval)
{
	copyQRectToPRect(((QStyleOptionTabBarBase *)handle)->selectedTabRect,retval);
}

void QStyleOptionTabBarBase_setSelectedTabRect(QStyleOptionTabBarBaseH handle, PRect selectedTabRect)
{
	copyPRectToQRect(selectedTabRect,((QStyleOptionTabBarBase *)handle)->selectedTabRect);
}

bool QStyleOptionTabBarBase_documentMode(QStyleOptionTabBarBaseH handle)
{
	return (bool) ((QStyleOptionTabBarBase *)handle)->documentMode;
}

void QStyleOptionTabBarBase_setDocumentMode(QStyleOptionTabBarBaseH handle, bool documentMode)
{
	((QStyleOptionTabBarBase *)handle)->documentMode = documentMode;
}

QStyleOptionTabBarBaseH QStyleOptionTabBarBase_Create()
{
	return (QStyleOptionTabBarBaseH) new QStyleOptionTabBarBase();
}

void QStyleOptionTabBarBase_Destroy(QStyleOptionTabBarBaseH handle)
{
	delete (QStyleOptionTabBarBase *)handle;
}

QStyleOptionTabBarBaseH QStyleOptionTabBarBase_Create2(const QStyleOptionTabBarBaseH other)
{
	return (QStyleOptionTabBarBaseH) new QStyleOptionTabBarBase(*(const QStyleOptionTabBarBase*)other);
}

int QStyleOptionHeader_section(QStyleOptionHeaderH handle)
{
	return (int) ((QStyleOptionHeader *)handle)->section;
}

void QStyleOptionHeader_setSection(QStyleOptionHeaderH handle, int section)
{
	((QStyleOptionHeader *)handle)->section = section;
}

void QStyleOptionHeader_text(QStyleOptionHeaderH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionHeader *)handle)->text,retval);
}

void QStyleOptionHeader_setText(QStyleOptionHeaderH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionHeader *)handle)->text);
}

unsigned int QStyleOptionHeader_textAlignment(QStyleOptionHeaderH handle)
{
	return (unsigned int) ((QStyleOptionHeader *)handle)->textAlignment;
}

void QStyleOptionHeader_setTextAlignment(QStyleOptionHeaderH handle, unsigned int textAlignment)
{
	((QStyleOptionHeader *)handle)->textAlignment =(Qt::Alignment)textAlignment;
}

void QStyleOptionHeader_icon(QStyleOptionHeaderH handle, QIconH retval)
{
	 *(QIcon *)retval = ((QStyleOptionHeader *)handle)->icon;
}

void QStyleOptionHeader_setIcon(QStyleOptionHeaderH handle, QIconH icon)
{
	((QStyleOptionHeader *)handle)->icon = *(QIcon *)icon;
}

unsigned int QStyleOptionHeader_iconAlignment(QStyleOptionHeaderH handle)
{
	return (unsigned int) ((QStyleOptionHeader *)handle)->iconAlignment;
}

void QStyleOptionHeader_setIconAlignment(QStyleOptionHeaderH handle, unsigned int iconAlignment)
{
	((QStyleOptionHeader *)handle)->iconAlignment =(Qt::Alignment)iconAlignment;
}

QStyleOptionHeader::SectionPosition QStyleOptionHeader_position(QStyleOptionHeaderH handle)
{
	return (QStyleOptionHeader::SectionPosition) ((QStyleOptionHeader *)handle)->position;
}

void QStyleOptionHeader_setPosition(QStyleOptionHeaderH handle, QStyleOptionHeader::SectionPosition position)
{
	((QStyleOptionHeader *)handle)->position = position;
}

QStyleOptionHeader::SelectedPosition QStyleOptionHeader_selectedPosition(QStyleOptionHeaderH handle)
{
	return (QStyleOptionHeader::SelectedPosition) ((QStyleOptionHeader *)handle)->selectedPosition;
}

void QStyleOptionHeader_setSelectedPosition(QStyleOptionHeaderH handle, QStyleOptionHeader::SelectedPosition selectedPosition)
{
	((QStyleOptionHeader *)handle)->selectedPosition = selectedPosition;
}

QStyleOptionHeader::SortIndicator QStyleOptionHeader_sortIndicator(QStyleOptionHeaderH handle)
{
	return (QStyleOptionHeader::SortIndicator) ((QStyleOptionHeader *)handle)->sortIndicator;
}

void QStyleOptionHeader_setSortIndicator(QStyleOptionHeaderH handle, QStyleOptionHeader::SortIndicator sortIndicator)
{
	((QStyleOptionHeader *)handle)->sortIndicator = sortIndicator;
}

Qt::Orientation QStyleOptionHeader_orientation(QStyleOptionHeaderH handle)
{
	return (Qt::Orientation) ((QStyleOptionHeader *)handle)->orientation;
}

void QStyleOptionHeader_setOrientation(QStyleOptionHeaderH handle, Qt::Orientation orientation)
{
	((QStyleOptionHeader *)handle)->orientation = orientation;
}

QStyleOptionHeaderH QStyleOptionHeader_Create()
{
	return (QStyleOptionHeaderH) new QStyleOptionHeader();
}

void QStyleOptionHeader_Destroy(QStyleOptionHeaderH handle)
{
	delete (QStyleOptionHeader *)handle;
}

QStyleOptionHeaderH QStyleOptionHeader_Create2(const QStyleOptionHeaderH other)
{
	return (QStyleOptionHeaderH) new QStyleOptionHeader(*(const QStyleOptionHeader*)other);
}

unsigned int QStyleOptionButton_features(QStyleOptionButtonH handle)
{
	return (unsigned int) ((QStyleOptionButton *)handle)->features;
}

void QStyleOptionButton_setFeatures(QStyleOptionButtonH handle, unsigned int features)
{
	((QStyleOptionButton *)handle)->features =(QStyleOptionButton::ButtonFeatures)features;
}

void QStyleOptionButton_text(QStyleOptionButtonH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionButton *)handle)->text,retval);
}

void QStyleOptionButton_setText(QStyleOptionButtonH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionButton *)handle)->text);
}

void QStyleOptionButton_icon(QStyleOptionButtonH handle, QIconH retval)
{
	 *(QIcon *)retval = ((QStyleOptionButton *)handle)->icon;
}

void QStyleOptionButton_setIcon(QStyleOptionButtonH handle, QIconH icon)
{
	((QStyleOptionButton *)handle)->icon = *(QIcon *)icon;
}

void QStyleOptionButton_iconSize(QStyleOptionButtonH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionButton *)handle)->iconSize;
}

void QStyleOptionButton_setIconSize(QStyleOptionButtonH handle, PSize iconSize)
{
	((QStyleOptionButton *)handle)->iconSize = *(QSize *)iconSize;
}

QStyleOptionButtonH QStyleOptionButton_Create()
{
	return (QStyleOptionButtonH) new QStyleOptionButton();
}

void QStyleOptionButton_Destroy(QStyleOptionButtonH handle)
{
	delete (QStyleOptionButton *)handle;
}

QStyleOptionButtonH QStyleOptionButton_Create2(const QStyleOptionButtonH other)
{
	return (QStyleOptionButtonH) new QStyleOptionButton(*(const QStyleOptionButton*)other);
}

QTabBar::Shape QStyleOptionTab_shape(QStyleOptionTabH handle)
{
	return (QTabBar::Shape) ((QStyleOptionTab *)handle)->shape;
}

void QStyleOptionTab_setShape(QStyleOptionTabH handle, QTabBar::Shape shape)
{
	((QStyleOptionTab *)handle)->shape = shape;
}

void QStyleOptionTab_text(QStyleOptionTabH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionTab *)handle)->text,retval);
}

void QStyleOptionTab_setText(QStyleOptionTabH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionTab *)handle)->text);
}

void QStyleOptionTab_icon(QStyleOptionTabH handle, QIconH retval)
{
	 *(QIcon *)retval = ((QStyleOptionTab *)handle)->icon;
}

void QStyleOptionTab_setIcon(QStyleOptionTabH handle, QIconH icon)
{
	((QStyleOptionTab *)handle)->icon = *(QIcon *)icon;
}

int QStyleOptionTab_row(QStyleOptionTabH handle)
{
	return (int) ((QStyleOptionTab *)handle)->row;
}

void QStyleOptionTab_setRow(QStyleOptionTabH handle, int row)
{
	((QStyleOptionTab *)handle)->row = row;
}

QStyleOptionTab::TabPosition QStyleOptionTab_position(QStyleOptionTabH handle)
{
	return (QStyleOptionTab::TabPosition) ((QStyleOptionTab *)handle)->position;
}

void QStyleOptionTab_setPosition(QStyleOptionTabH handle, QStyleOptionTab::TabPosition position)
{
	((QStyleOptionTab *)handle)->position = position;
}

QStyleOptionTab::SelectedPosition QStyleOptionTab_selectedPosition(QStyleOptionTabH handle)
{
	return (QStyleOptionTab::SelectedPosition) ((QStyleOptionTab *)handle)->selectedPosition;
}

void QStyleOptionTab_setSelectedPosition(QStyleOptionTabH handle, QStyleOptionTab::SelectedPosition selectedPosition)
{
	((QStyleOptionTab *)handle)->selectedPosition = selectedPosition;
}

unsigned int QStyleOptionTab_cornerWidgets(QStyleOptionTabH handle)
{
	return (unsigned int) ((QStyleOptionTab *)handle)->cornerWidgets;
}

void QStyleOptionTab_setCornerWidgets(QStyleOptionTabH handle, unsigned int cornerWidgets)
{
	((QStyleOptionTab *)handle)->cornerWidgets =(QStyleOptionTab::CornerWidgets)cornerWidgets;
}

void QStyleOptionTab_iconSize(QStyleOptionTabH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionTab *)handle)->iconSize;
}

void QStyleOptionTab_setIconSize(QStyleOptionTabH handle, PSize iconSize)
{
	((QStyleOptionTab *)handle)->iconSize = *(QSize *)iconSize;
}

bool QStyleOptionTab_documentMode(QStyleOptionTabH handle)
{
	return (bool) ((QStyleOptionTab *)handle)->documentMode;
}

void QStyleOptionTab_setDocumentMode(QStyleOptionTabH handle, bool documentMode)
{
	((QStyleOptionTab *)handle)->documentMode = documentMode;
}

void QStyleOptionTab_leftButtonSize(QStyleOptionTabH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionTab *)handle)->leftButtonSize;
}

void QStyleOptionTab_setLeftButtonSize(QStyleOptionTabH handle, PSize leftButtonSize)
{
	((QStyleOptionTab *)handle)->leftButtonSize = *(QSize *)leftButtonSize;
}

void QStyleOptionTab_rightButtonSize(QStyleOptionTabH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionTab *)handle)->rightButtonSize;
}

void QStyleOptionTab_setRightButtonSize(QStyleOptionTabH handle, PSize rightButtonSize)
{
	((QStyleOptionTab *)handle)->rightButtonSize = *(QSize *)rightButtonSize;
}

unsigned int QStyleOptionTab_features(QStyleOptionTabH handle)
{
	return (unsigned int) ((QStyleOptionTab *)handle)->features;
}

void QStyleOptionTab_setFeatures(QStyleOptionTabH handle, unsigned int features)
{
	((QStyleOptionTab *)handle)->features =(QStyleOptionTab::TabFeatures)features;
}

QStyleOptionTabH QStyleOptionTab_Create()
{
	return (QStyleOptionTabH) new QStyleOptionTab();
}

void QStyleOptionTab_Destroy(QStyleOptionTabH handle)
{
	delete (QStyleOptionTab *)handle;
}

QStyleOptionTabH QStyleOptionTab_Create2(const QStyleOptionTabH other)
{
	return (QStyleOptionTabH) new QStyleOptionTab(*(const QStyleOptionTab*)other);
}

QStyleOptionToolBar::ToolBarPosition QStyleOptionToolBar_positionOfLine(QStyleOptionToolBarH handle)
{
	return (QStyleOptionToolBar::ToolBarPosition) ((QStyleOptionToolBar *)handle)->positionOfLine;
}

void QStyleOptionToolBar_setPositionOfLine(QStyleOptionToolBarH handle, QStyleOptionToolBar::ToolBarPosition positionOfLine)
{
	((QStyleOptionToolBar *)handle)->positionOfLine = positionOfLine;
}

QStyleOptionToolBar::ToolBarPosition QStyleOptionToolBar_positionWithinLine(QStyleOptionToolBarH handle)
{
	return (QStyleOptionToolBar::ToolBarPosition) ((QStyleOptionToolBar *)handle)->positionWithinLine;
}

void QStyleOptionToolBar_setPositionWithinLine(QStyleOptionToolBarH handle, QStyleOptionToolBar::ToolBarPosition positionWithinLine)
{
	((QStyleOptionToolBar *)handle)->positionWithinLine = positionWithinLine;
}

Qt::ToolBarArea QStyleOptionToolBar_toolBarArea(QStyleOptionToolBarH handle)
{
	return (Qt::ToolBarArea) ((QStyleOptionToolBar *)handle)->toolBarArea;
}

void QStyleOptionToolBar_setToolBarArea(QStyleOptionToolBarH handle, Qt::ToolBarArea toolBarArea)
{
	((QStyleOptionToolBar *)handle)->toolBarArea = toolBarArea;
}

unsigned int QStyleOptionToolBar_features(QStyleOptionToolBarH handle)
{
	return (unsigned int) ((QStyleOptionToolBar *)handle)->features;
}

void QStyleOptionToolBar_setFeatures(QStyleOptionToolBarH handle, unsigned int features)
{
	((QStyleOptionToolBar *)handle)->features =(QStyleOptionToolBar::ToolBarFeatures)features;
}

int QStyleOptionToolBar_lineWidth(QStyleOptionToolBarH handle)
{
	return (int) ((QStyleOptionToolBar *)handle)->lineWidth;
}

void QStyleOptionToolBar_setLineWidth(QStyleOptionToolBarH handle, int lineWidth)
{
	((QStyleOptionToolBar *)handle)->lineWidth = lineWidth;
}

int QStyleOptionToolBar_midLineWidth(QStyleOptionToolBarH handle)
{
	return (int) ((QStyleOptionToolBar *)handle)->midLineWidth;
}

void QStyleOptionToolBar_setMidLineWidth(QStyleOptionToolBarH handle, int midLineWidth)
{
	((QStyleOptionToolBar *)handle)->midLineWidth = midLineWidth;
}

QStyleOptionToolBarH QStyleOptionToolBar_Create()
{
	return (QStyleOptionToolBarH) new QStyleOptionToolBar();
}

void QStyleOptionToolBar_Destroy(QStyleOptionToolBarH handle)
{
	delete (QStyleOptionToolBar *)handle;
}

QStyleOptionToolBarH QStyleOptionToolBar_Create2(const QStyleOptionToolBarH other)
{
	return (QStyleOptionToolBarH) new QStyleOptionToolBar(*(const QStyleOptionToolBar*)other);
}

int QStyleOptionProgressBar_minimum(QStyleOptionProgressBarH handle)
{
	return (int) ((QStyleOptionProgressBar *)handle)->minimum;
}

void QStyleOptionProgressBar_setMinimum(QStyleOptionProgressBarH handle, int minimum)
{
	((QStyleOptionProgressBar *)handle)->minimum = minimum;
}

int QStyleOptionProgressBar_maximum(QStyleOptionProgressBarH handle)
{
	return (int) ((QStyleOptionProgressBar *)handle)->maximum;
}

void QStyleOptionProgressBar_setMaximum(QStyleOptionProgressBarH handle, int maximum)
{
	((QStyleOptionProgressBar *)handle)->maximum = maximum;
}

int QStyleOptionProgressBar_progress(QStyleOptionProgressBarH handle)
{
	return (int) ((QStyleOptionProgressBar *)handle)->progress;
}

void QStyleOptionProgressBar_setProgress(QStyleOptionProgressBarH handle, int progress)
{
	((QStyleOptionProgressBar *)handle)->progress = progress;
}

void QStyleOptionProgressBar_text(QStyleOptionProgressBarH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionProgressBar *)handle)->text,retval);
}

void QStyleOptionProgressBar_setText(QStyleOptionProgressBarH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionProgressBar *)handle)->text);
}

unsigned int QStyleOptionProgressBar_textAlignment(QStyleOptionProgressBarH handle)
{
	return (unsigned int) ((QStyleOptionProgressBar *)handle)->textAlignment;
}

void QStyleOptionProgressBar_setTextAlignment(QStyleOptionProgressBarH handle, unsigned int textAlignment)
{
	((QStyleOptionProgressBar *)handle)->textAlignment =(Qt::Alignment)textAlignment;
}

bool QStyleOptionProgressBar_textVisible(QStyleOptionProgressBarH handle)
{
	return (bool) ((QStyleOptionProgressBar *)handle)->textVisible;
}

void QStyleOptionProgressBar_setTextVisible(QStyleOptionProgressBarH handle, bool textVisible)
{
	((QStyleOptionProgressBar *)handle)->textVisible = textVisible;
}

bool QStyleOptionProgressBar_invertedAppearance(QStyleOptionProgressBarH handle)
{
	return (bool) ((QStyleOptionProgressBar *)handle)->invertedAppearance;
}

void QStyleOptionProgressBar_setInvertedAppearance(QStyleOptionProgressBarH handle, bool invertedAppearance)
{
	((QStyleOptionProgressBar *)handle)->invertedAppearance = invertedAppearance;
}

bool QStyleOptionProgressBar_bottomToTop(QStyleOptionProgressBarH handle)
{
	return (bool) ((QStyleOptionProgressBar *)handle)->bottomToTop;
}

void QStyleOptionProgressBar_setBottomToTop(QStyleOptionProgressBarH handle, bool bottomToTop)
{
	((QStyleOptionProgressBar *)handle)->bottomToTop = bottomToTop;
}

QStyleOptionProgressBarH QStyleOptionProgressBar_Create()
{
	return (QStyleOptionProgressBarH) new QStyleOptionProgressBar();
}

void QStyleOptionProgressBar_Destroy(QStyleOptionProgressBarH handle)
{
	delete (QStyleOptionProgressBar *)handle;
}

QStyleOptionProgressBarH QStyleOptionProgressBar_Create2(const QStyleOptionProgressBarH other)
{
	return (QStyleOptionProgressBarH) new QStyleOptionProgressBar(*(const QStyleOptionProgressBar*)other);
}

QStyleOptionMenuItem::MenuItemType QStyleOptionMenuItem_menuItemType(QStyleOptionMenuItemH handle)
{
	return (QStyleOptionMenuItem::MenuItemType) ((QStyleOptionMenuItem *)handle)->menuItemType;
}

void QStyleOptionMenuItem_setMenuItemType(QStyleOptionMenuItemH handle, QStyleOptionMenuItem::MenuItemType menuItemType)
{
	((QStyleOptionMenuItem *)handle)->menuItemType = menuItemType;
}

QStyleOptionMenuItem::CheckType QStyleOptionMenuItem_checkType(QStyleOptionMenuItemH handle)
{
	return (QStyleOptionMenuItem::CheckType) ((QStyleOptionMenuItem *)handle)->checkType;
}

void QStyleOptionMenuItem_setCheckType(QStyleOptionMenuItemH handle, QStyleOptionMenuItem::CheckType checkType)
{
	((QStyleOptionMenuItem *)handle)->checkType = checkType;
}

bool QStyleOptionMenuItem_checked(QStyleOptionMenuItemH handle)
{
	return (bool) ((QStyleOptionMenuItem *)handle)->checked;
}

void QStyleOptionMenuItem_setChecked(QStyleOptionMenuItemH handle, bool checked)
{
	((QStyleOptionMenuItem *)handle)->checked = checked;
}

bool QStyleOptionMenuItem_menuHasCheckableItems(QStyleOptionMenuItemH handle)
{
	return (bool) ((QStyleOptionMenuItem *)handle)->menuHasCheckableItems;
}

void QStyleOptionMenuItem_setMenuHasCheckableItems(QStyleOptionMenuItemH handle, bool menuHasCheckableItems)
{
	((QStyleOptionMenuItem *)handle)->menuHasCheckableItems = menuHasCheckableItems;
}

void QStyleOptionMenuItem_menuRect(QStyleOptionMenuItemH handle, PRect retval)
{
	copyQRectToPRect(((QStyleOptionMenuItem *)handle)->menuRect,retval);
}

void QStyleOptionMenuItem_setMenuRect(QStyleOptionMenuItemH handle, PRect menuRect)
{
	copyPRectToQRect(menuRect,((QStyleOptionMenuItem *)handle)->menuRect);
}

void QStyleOptionMenuItem_text(QStyleOptionMenuItemH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionMenuItem *)handle)->text,retval);
}

void QStyleOptionMenuItem_setText(QStyleOptionMenuItemH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionMenuItem *)handle)->text);
}

void QStyleOptionMenuItem_icon(QStyleOptionMenuItemH handle, QIconH retval)
{
	 *(QIcon *)retval = ((QStyleOptionMenuItem *)handle)->icon;
}

void QStyleOptionMenuItem_setIcon(QStyleOptionMenuItemH handle, QIconH icon)
{
	((QStyleOptionMenuItem *)handle)->icon = *(QIcon *)icon;
}

int QStyleOptionMenuItem_maxIconWidth(QStyleOptionMenuItemH handle)
{
	return (int) ((QStyleOptionMenuItem *)handle)->maxIconWidth;
}

void QStyleOptionMenuItem_setMaxIconWidth(QStyleOptionMenuItemH handle, int maxIconWidth)
{
	((QStyleOptionMenuItem *)handle)->maxIconWidth = maxIconWidth;
}

void QStyleOptionMenuItem_font(QStyleOptionMenuItemH handle, QFontH retval)
{
	 *(QFont *)retval = ((QStyleOptionMenuItem *)handle)->font;
}

void QStyleOptionMenuItem_setFont(QStyleOptionMenuItemH handle, QFontH font)
{
	((QStyleOptionMenuItem *)handle)->font = *(QFont *)font;
}

QStyleOptionMenuItemH QStyleOptionMenuItem_Create()
{
	return (QStyleOptionMenuItemH) new QStyleOptionMenuItem();
}

void QStyleOptionMenuItem_Destroy(QStyleOptionMenuItemH handle)
{
	delete (QStyleOptionMenuItem *)handle;
}

QStyleOptionMenuItemH QStyleOptionMenuItem_Create2(const QStyleOptionMenuItemH other)
{
	return (QStyleOptionMenuItemH) new QStyleOptionMenuItem(*(const QStyleOptionMenuItem*)other);
}

void QStyleOptionDockWidget_title(QStyleOptionDockWidgetH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionDockWidget *)handle)->title,retval);
}

void QStyleOptionDockWidget_setTitle(QStyleOptionDockWidgetH handle, PWideString title)
{
	copyPWideStringToQString(title,((QStyleOptionDockWidget *)handle)->title);
}

bool QStyleOptionDockWidget_closable(QStyleOptionDockWidgetH handle)
{
	return (bool) ((QStyleOptionDockWidget *)handle)->closable;
}

void QStyleOptionDockWidget_setClosable(QStyleOptionDockWidgetH handle, bool closable)
{
	((QStyleOptionDockWidget *)handle)->closable = closable;
}

bool QStyleOptionDockWidget_movable(QStyleOptionDockWidgetH handle)
{
	return (bool) ((QStyleOptionDockWidget *)handle)->movable;
}

void QStyleOptionDockWidget_setMovable(QStyleOptionDockWidgetH handle, bool movable)
{
	((QStyleOptionDockWidget *)handle)->movable = movable;
}

bool QStyleOptionDockWidget_floatable(QStyleOptionDockWidgetH handle)
{
	return (bool) ((QStyleOptionDockWidget *)handle)->floatable;
}

void QStyleOptionDockWidget_setFloatable(QStyleOptionDockWidgetH handle, bool floatable)
{
	((QStyleOptionDockWidget *)handle)->floatable = floatable;
}

bool QStyleOptionDockWidget_verticalTitleBar(QStyleOptionDockWidgetH handle)
{
	return (bool) ((QStyleOptionDockWidget *)handle)->verticalTitleBar;
}

void QStyleOptionDockWidget_setVerticalTitleBar(QStyleOptionDockWidgetH handle, bool verticalTitleBar)
{
	((QStyleOptionDockWidget *)handle)->verticalTitleBar = verticalTitleBar;
}

QStyleOptionDockWidgetH QStyleOptionDockWidget_Create()
{
	return (QStyleOptionDockWidgetH) new QStyleOptionDockWidget();
}

void QStyleOptionDockWidget_Destroy(QStyleOptionDockWidgetH handle)
{
	delete (QStyleOptionDockWidget *)handle;
}

QStyleOptionDockWidgetH QStyleOptionDockWidget_Create2(const QStyleOptionDockWidgetH other)
{
	return (QStyleOptionDockWidgetH) new QStyleOptionDockWidget(*(const QStyleOptionDockWidget*)other);
}

unsigned int QStyleOptionViewItem_displayAlignment(QStyleOptionViewItemH handle)
{
	return (unsigned int) ((QStyleOptionViewItem *)handle)->displayAlignment;
}

void QStyleOptionViewItem_setDisplayAlignment(QStyleOptionViewItemH handle, unsigned int displayAlignment)
{
	((QStyleOptionViewItem *)handle)->displayAlignment =(Qt::Alignment)displayAlignment;
}

unsigned int QStyleOptionViewItem_decorationAlignment(QStyleOptionViewItemH handle)
{
	return (unsigned int) ((QStyleOptionViewItem *)handle)->decorationAlignment;
}

void QStyleOptionViewItem_setDecorationAlignment(QStyleOptionViewItemH handle, unsigned int decorationAlignment)
{
	((QStyleOptionViewItem *)handle)->decorationAlignment =(Qt::Alignment)decorationAlignment;
}

Qt::TextElideMode QStyleOptionViewItem_textElideMode(QStyleOptionViewItemH handle)
{
	return (Qt::TextElideMode) ((QStyleOptionViewItem *)handle)->textElideMode;
}

void QStyleOptionViewItem_setTextElideMode(QStyleOptionViewItemH handle, Qt::TextElideMode textElideMode)
{
	((QStyleOptionViewItem *)handle)->textElideMode = textElideMode;
}

QStyleOptionViewItem::Position QStyleOptionViewItem_decorationPosition(QStyleOptionViewItemH handle)
{
	return (QStyleOptionViewItem::Position) ((QStyleOptionViewItem *)handle)->decorationPosition;
}

void QStyleOptionViewItem_setDecorationPosition(QStyleOptionViewItemH handle, QStyleOptionViewItem::Position decorationPosition)
{
	((QStyleOptionViewItem *)handle)->decorationPosition = decorationPosition;
}

void QStyleOptionViewItem_decorationSize(QStyleOptionViewItemH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionViewItem *)handle)->decorationSize;
}

void QStyleOptionViewItem_setDecorationSize(QStyleOptionViewItemH handle, PSize decorationSize)
{
	((QStyleOptionViewItem *)handle)->decorationSize = *(QSize *)decorationSize;
}

void QStyleOptionViewItem_font(QStyleOptionViewItemH handle, QFontH retval)
{
	 *(QFont *)retval = ((QStyleOptionViewItem *)handle)->font;
}

void QStyleOptionViewItem_setFont(QStyleOptionViewItemH handle, QFontH font)
{
	((QStyleOptionViewItem *)handle)->font = *(QFont *)font;
}

bool QStyleOptionViewItem_showDecorationSelected(QStyleOptionViewItemH handle)
{
	return (bool) ((QStyleOptionViewItem *)handle)->showDecorationSelected;
}

void QStyleOptionViewItem_setShowDecorationSelected(QStyleOptionViewItemH handle, bool showDecorationSelected)
{
	((QStyleOptionViewItem *)handle)->showDecorationSelected = showDecorationSelected;
}

unsigned int QStyleOptionViewItem_features(QStyleOptionViewItemH handle)
{
	return (unsigned int) ((QStyleOptionViewItem *)handle)->features;
}

void QStyleOptionViewItem_setFeatures(QStyleOptionViewItemH handle, unsigned int features)
{
	((QStyleOptionViewItem *)handle)->features =(QStyleOptionViewItem::ViewItemFeatures)features;
}

void QStyleOptionViewItem_locale(QStyleOptionViewItemH handle, QLocaleH retval)
{
	 *(QLocale *)retval = ((QStyleOptionViewItem *)handle)->locale;
}

void QStyleOptionViewItem_setLocale(QStyleOptionViewItemH handle, QLocaleH locale)
{
	((QStyleOptionViewItem *)handle)->locale = *(QLocale *)locale;
}

const QWidgetH QStyleOptionViewItem_widget(QStyleOptionViewItemH handle)
{
	return (const QWidgetH) ((QStyleOptionViewItem *)handle)->widget;
}

void QStyleOptionViewItem_setWidget(QStyleOptionViewItemH handle, const QWidgetH widget)
{
	((QStyleOptionViewItem *)handle)->widget =(const QWidget *)widget;
}

void QStyleOptionViewItem_index(QStyleOptionViewItemH handle, QModelIndexH retval)
{
	 *(QModelIndex *)retval = ((QStyleOptionViewItem *)handle)->index;
}

void QStyleOptionViewItem_setIndex(QStyleOptionViewItemH handle, QModelIndexH index)
{
	((QStyleOptionViewItem *)handle)->index = *(QModelIndex *)index;
}

Qt::CheckState QStyleOptionViewItem_checkState(QStyleOptionViewItemH handle)
{
	return (Qt::CheckState) ((QStyleOptionViewItem *)handle)->checkState;
}

void QStyleOptionViewItem_setCheckState(QStyleOptionViewItemH handle, Qt::CheckState checkState)
{
	((QStyleOptionViewItem *)handle)->checkState = checkState;
}

void QStyleOptionViewItem_icon(QStyleOptionViewItemH handle, QIconH retval)
{
	 *(QIcon *)retval = ((QStyleOptionViewItem *)handle)->icon;
}

void QStyleOptionViewItem_setIcon(QStyleOptionViewItemH handle, QIconH icon)
{
	((QStyleOptionViewItem *)handle)->icon = *(QIcon *)icon;
}

void QStyleOptionViewItem_text(QStyleOptionViewItemH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionViewItem *)handle)->text,retval);
}

void QStyleOptionViewItem_setText(QStyleOptionViewItemH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionViewItem *)handle)->text);
}

QStyleOptionViewItem::ViewItemPosition QStyleOptionViewItem_viewItemPosition(QStyleOptionViewItemH handle)
{
	return (QStyleOptionViewItem::ViewItemPosition) ((QStyleOptionViewItem *)handle)->viewItemPosition;
}

void QStyleOptionViewItem_setViewItemPosition(QStyleOptionViewItemH handle, QStyleOptionViewItem::ViewItemPosition viewItemPosition)
{
	((QStyleOptionViewItem *)handle)->viewItemPosition = viewItemPosition;
}

void QStyleOptionViewItem_backgroundBrush(QStyleOptionViewItemH handle, QBrushH retval)
{
	 *(QBrush *)retval = ((QStyleOptionViewItem *)handle)->backgroundBrush;
}

void QStyleOptionViewItem_setBackgroundBrush(QStyleOptionViewItemH handle, QBrushH backgroundBrush)
{
	((QStyleOptionViewItem *)handle)->backgroundBrush = *(QBrush *)backgroundBrush;
}

QStyleOptionViewItemH QStyleOptionViewItem_Create()
{
	return (QStyleOptionViewItemH) new QStyleOptionViewItem();
}

void QStyleOptionViewItem_Destroy(QStyleOptionViewItemH handle)
{
	delete (QStyleOptionViewItem *)handle;
}

QStyleOptionViewItemH QStyleOptionViewItem_Create2(const QStyleOptionViewItemH other)
{
	return (QStyleOptionViewItemH) new QStyleOptionViewItem(*(const QStyleOptionViewItem*)other);
}

void QStyleOptionToolBox_text(QStyleOptionToolBoxH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionToolBox *)handle)->text,retval);
}

void QStyleOptionToolBox_setText(QStyleOptionToolBoxH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionToolBox *)handle)->text);
}

void QStyleOptionToolBox_icon(QStyleOptionToolBoxH handle, QIconH retval)
{
	 *(QIcon *)retval = ((QStyleOptionToolBox *)handle)->icon;
}

void QStyleOptionToolBox_setIcon(QStyleOptionToolBoxH handle, QIconH icon)
{
	((QStyleOptionToolBox *)handle)->icon = *(QIcon *)icon;
}

QStyleOptionToolBox::TabPosition QStyleOptionToolBox_position(QStyleOptionToolBoxH handle)
{
	return (QStyleOptionToolBox::TabPosition) ((QStyleOptionToolBox *)handle)->position;
}

void QStyleOptionToolBox_setPosition(QStyleOptionToolBoxH handle, QStyleOptionToolBox::TabPosition position)
{
	((QStyleOptionToolBox *)handle)->position = position;
}

QStyleOptionToolBox::SelectedPosition QStyleOptionToolBox_selectedPosition(QStyleOptionToolBoxH handle)
{
	return (QStyleOptionToolBox::SelectedPosition) ((QStyleOptionToolBox *)handle)->selectedPosition;
}

void QStyleOptionToolBox_setSelectedPosition(QStyleOptionToolBoxH handle, QStyleOptionToolBox::SelectedPosition selectedPosition)
{
	((QStyleOptionToolBox *)handle)->selectedPosition = selectedPosition;
}

QStyleOptionToolBoxH QStyleOptionToolBox_Create()
{
	return (QStyleOptionToolBoxH) new QStyleOptionToolBox();
}

void QStyleOptionToolBox_Destroy(QStyleOptionToolBoxH handle)
{
	delete (QStyleOptionToolBox *)handle;
}

QStyleOptionToolBoxH QStyleOptionToolBox_Create2(const QStyleOptionToolBoxH other)
{
	return (QStyleOptionToolBoxH) new QStyleOptionToolBox(*(const QStyleOptionToolBox*)other);
}

QRubberBand::Shape QStyleOptionRubberBand_shape(QStyleOptionRubberBandH handle)
{
	return (QRubberBand::Shape) ((QStyleOptionRubberBand *)handle)->shape;
}

void QStyleOptionRubberBand_setShape(QStyleOptionRubberBandH handle, QRubberBand::Shape shape)
{
	((QStyleOptionRubberBand *)handle)->shape = shape;
}

bool QStyleOptionRubberBand_opaque(QStyleOptionRubberBandH handle)
{
	return (bool) ((QStyleOptionRubberBand *)handle)->opaque;
}

void QStyleOptionRubberBand_setOpaque(QStyleOptionRubberBandH handle, bool opaque)
{
	((QStyleOptionRubberBand *)handle)->opaque = opaque;
}

QStyleOptionRubberBandH QStyleOptionRubberBand_Create()
{
	return (QStyleOptionRubberBandH) new QStyleOptionRubberBand();
}

void QStyleOptionRubberBand_Destroy(QStyleOptionRubberBandH handle)
{
	delete (QStyleOptionRubberBand *)handle;
}

QStyleOptionRubberBandH QStyleOptionRubberBand_Create2(const QStyleOptionRubberBandH other)
{
	return (QStyleOptionRubberBandH) new QStyleOptionRubberBand(*(const QStyleOptionRubberBand*)other);
}

unsigned int QStyleOptionComplex_subControls(QStyleOptionComplexH handle)
{
	return (unsigned int) ((QStyleOptionComplex *)handle)->subControls;
}

void QStyleOptionComplex_setSubControls(QStyleOptionComplexH handle, unsigned int subControls)
{
	((QStyleOptionComplex *)handle)->subControls =(QStyle::SubControls)subControls;
}

unsigned int QStyleOptionComplex_activeSubControls(QStyleOptionComplexH handle)
{
	return (unsigned int) ((QStyleOptionComplex *)handle)->activeSubControls;
}

void QStyleOptionComplex_setActiveSubControls(QStyleOptionComplexH handle, unsigned int activeSubControls)
{
	((QStyleOptionComplex *)handle)->activeSubControls =(QStyle::SubControls)activeSubControls;
}

QStyleOptionComplexH QStyleOptionComplex_Create(int version, int type)
{
	return (QStyleOptionComplexH) new QStyleOptionComplex(version, type);
}

void QStyleOptionComplex_Destroy(QStyleOptionComplexH handle)
{
	delete (QStyleOptionComplex *)handle;
}

QStyleOptionComplexH QStyleOptionComplex_Create2(const QStyleOptionComplexH other)
{
	return (QStyleOptionComplexH) new QStyleOptionComplex(*(const QStyleOptionComplex*)other);
}

Qt::Orientation QStyleOptionSlider_orientation(QStyleOptionSliderH handle)
{
	return (Qt::Orientation) ((QStyleOptionSlider *)handle)->orientation;
}

void QStyleOptionSlider_setOrientation(QStyleOptionSliderH handle, Qt::Orientation orientation)
{
	((QStyleOptionSlider *)handle)->orientation = orientation;
}

int QStyleOptionSlider_minimum(QStyleOptionSliderH handle)
{
	return (int) ((QStyleOptionSlider *)handle)->minimum;
}

void QStyleOptionSlider_setMinimum(QStyleOptionSliderH handle, int minimum)
{
	((QStyleOptionSlider *)handle)->minimum = minimum;
}

int QStyleOptionSlider_maximum(QStyleOptionSliderH handle)
{
	return (int) ((QStyleOptionSlider *)handle)->maximum;
}

void QStyleOptionSlider_setMaximum(QStyleOptionSliderH handle, int maximum)
{
	((QStyleOptionSlider *)handle)->maximum = maximum;
}

QSlider::TickPosition QStyleOptionSlider_tickPosition(QStyleOptionSliderH handle)
{
	return (QSlider::TickPosition) ((QStyleOptionSlider *)handle)->tickPosition;
}

void QStyleOptionSlider_setTickPosition(QStyleOptionSliderH handle, QSlider::TickPosition tickPosition)
{
	((QStyleOptionSlider *)handle)->tickPosition = tickPosition;
}

int QStyleOptionSlider_tickInterval(QStyleOptionSliderH handle)
{
	return (int) ((QStyleOptionSlider *)handle)->tickInterval;
}

void QStyleOptionSlider_setTickInterval(QStyleOptionSliderH handle, int tickInterval)
{
	((QStyleOptionSlider *)handle)->tickInterval = tickInterval;
}

bool QStyleOptionSlider_upsideDown(QStyleOptionSliderH handle)
{
	return (bool) ((QStyleOptionSlider *)handle)->upsideDown;
}

void QStyleOptionSlider_setUpsideDown(QStyleOptionSliderH handle, bool upsideDown)
{
	((QStyleOptionSlider *)handle)->upsideDown = upsideDown;
}

int QStyleOptionSlider_sliderPosition(QStyleOptionSliderH handle)
{
	return (int) ((QStyleOptionSlider *)handle)->sliderPosition;
}

void QStyleOptionSlider_setSliderPosition(QStyleOptionSliderH handle, int sliderPosition)
{
	((QStyleOptionSlider *)handle)->sliderPosition = sliderPosition;
}

int QStyleOptionSlider_sliderValue(QStyleOptionSliderH handle)
{
	return (int) ((QStyleOptionSlider *)handle)->sliderValue;
}

void QStyleOptionSlider_setSliderValue(QStyleOptionSliderH handle, int sliderValue)
{
	((QStyleOptionSlider *)handle)->sliderValue = sliderValue;
}

int QStyleOptionSlider_singleStep(QStyleOptionSliderH handle)
{
	return (int) ((QStyleOptionSlider *)handle)->singleStep;
}

void QStyleOptionSlider_setSingleStep(QStyleOptionSliderH handle, int singleStep)
{
	((QStyleOptionSlider *)handle)->singleStep = singleStep;
}

int QStyleOptionSlider_pageStep(QStyleOptionSliderH handle)
{
	return (int) ((QStyleOptionSlider *)handle)->pageStep;
}

void QStyleOptionSlider_setPageStep(QStyleOptionSliderH handle, int pageStep)
{
	((QStyleOptionSlider *)handle)->pageStep = pageStep;
}

qreal QStyleOptionSlider_notchTarget(QStyleOptionSliderH handle)
{
	return (qreal) ((QStyleOptionSlider *)handle)->notchTarget;
}

void QStyleOptionSlider_setNotchTarget(QStyleOptionSliderH handle, qreal notchTarget)
{
	((QStyleOptionSlider *)handle)->notchTarget = notchTarget;
}

bool QStyleOptionSlider_dialWrapping(QStyleOptionSliderH handle)
{
	return (bool) ((QStyleOptionSlider *)handle)->dialWrapping;
}

void QStyleOptionSlider_setDialWrapping(QStyleOptionSliderH handle, bool dialWrapping)
{
	((QStyleOptionSlider *)handle)->dialWrapping = dialWrapping;
}

QStyleOptionSliderH QStyleOptionSlider_Create()
{
	return (QStyleOptionSliderH) new QStyleOptionSlider();
}

void QStyleOptionSlider_Destroy(QStyleOptionSliderH handle)
{
	delete (QStyleOptionSlider *)handle;
}

QStyleOptionSliderH QStyleOptionSlider_Create2(const QStyleOptionSliderH other)
{
	return (QStyleOptionSliderH) new QStyleOptionSlider(*(const QStyleOptionSlider*)other);
}

QAbstractSpinBox::ButtonSymbols QStyleOptionSpinBox_buttonSymbols(QStyleOptionSpinBoxH handle)
{
	return (QAbstractSpinBox::ButtonSymbols) ((QStyleOptionSpinBox *)handle)->buttonSymbols;
}

void QStyleOptionSpinBox_setButtonSymbols(QStyleOptionSpinBoxH handle, QAbstractSpinBox::ButtonSymbols buttonSymbols)
{
	((QStyleOptionSpinBox *)handle)->buttonSymbols = buttonSymbols;
}

unsigned int QStyleOptionSpinBox_stepEnabled(QStyleOptionSpinBoxH handle)
{
	return (unsigned int) ((QStyleOptionSpinBox *)handle)->stepEnabled;
}

void QStyleOptionSpinBox_setStepEnabled(QStyleOptionSpinBoxH handle, unsigned int stepEnabled)
{
	((QStyleOptionSpinBox *)handle)->stepEnabled =(QAbstractSpinBox::StepEnabled)stepEnabled;
}

bool QStyleOptionSpinBox_frame(QStyleOptionSpinBoxH handle)
{
	return (bool) ((QStyleOptionSpinBox *)handle)->frame;
}

void QStyleOptionSpinBox_setFrame(QStyleOptionSpinBoxH handle, bool frame)
{
	((QStyleOptionSpinBox *)handle)->frame = frame;
}

QStyleOptionSpinBoxH QStyleOptionSpinBox_Create()
{
	return (QStyleOptionSpinBoxH) new QStyleOptionSpinBox();
}

void QStyleOptionSpinBox_Destroy(QStyleOptionSpinBoxH handle)
{
	delete (QStyleOptionSpinBox *)handle;
}

QStyleOptionSpinBoxH QStyleOptionSpinBox_Create2(const QStyleOptionSpinBoxH other)
{
	return (QStyleOptionSpinBoxH) new QStyleOptionSpinBox(*(const QStyleOptionSpinBox*)other);
}

unsigned int QStyleOptionToolButton_features(QStyleOptionToolButtonH handle)
{
	return (unsigned int) ((QStyleOptionToolButton *)handle)->features;
}

void QStyleOptionToolButton_setFeatures(QStyleOptionToolButtonH handle, unsigned int features)
{
	((QStyleOptionToolButton *)handle)->features =(QStyleOptionToolButton::ToolButtonFeatures)features;
}

void QStyleOptionToolButton_icon(QStyleOptionToolButtonH handle, QIconH retval)
{
	 *(QIcon *)retval = ((QStyleOptionToolButton *)handle)->icon;
}

void QStyleOptionToolButton_setIcon(QStyleOptionToolButtonH handle, QIconH icon)
{
	((QStyleOptionToolButton *)handle)->icon = *(QIcon *)icon;
}

void QStyleOptionToolButton_iconSize(QStyleOptionToolButtonH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionToolButton *)handle)->iconSize;
}

void QStyleOptionToolButton_setIconSize(QStyleOptionToolButtonH handle, PSize iconSize)
{
	((QStyleOptionToolButton *)handle)->iconSize = *(QSize *)iconSize;
}

void QStyleOptionToolButton_text(QStyleOptionToolButtonH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionToolButton *)handle)->text,retval);
}

void QStyleOptionToolButton_setText(QStyleOptionToolButtonH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionToolButton *)handle)->text);
}

Qt::ArrowType QStyleOptionToolButton_arrowType(QStyleOptionToolButtonH handle)
{
	return (Qt::ArrowType) ((QStyleOptionToolButton *)handle)->arrowType;
}

void QStyleOptionToolButton_setArrowType(QStyleOptionToolButtonH handle, Qt::ArrowType arrowType)
{
	((QStyleOptionToolButton *)handle)->arrowType = arrowType;
}

Qt::ToolButtonStyle QStyleOptionToolButton_toolButtonStyle(QStyleOptionToolButtonH handle)
{
	return (Qt::ToolButtonStyle) ((QStyleOptionToolButton *)handle)->toolButtonStyle;
}

void QStyleOptionToolButton_setToolButtonStyle(QStyleOptionToolButtonH handle, Qt::ToolButtonStyle toolButtonStyle)
{
	((QStyleOptionToolButton *)handle)->toolButtonStyle = toolButtonStyle;
}

void QStyleOptionToolButton_pos(QStyleOptionToolButtonH handle, PQtPoint retval)
{
	 *(QPoint *)retval = ((QStyleOptionToolButton *)handle)->pos;
}

void QStyleOptionToolButton_setPos(QStyleOptionToolButtonH handle, PQtPoint pos)
{
	((QStyleOptionToolButton *)handle)->pos = *(QPoint *)pos;
}

void QStyleOptionToolButton_font(QStyleOptionToolButtonH handle, QFontH retval)
{
	 *(QFont *)retval = ((QStyleOptionToolButton *)handle)->font;
}

void QStyleOptionToolButton_setFont(QStyleOptionToolButtonH handle, QFontH font)
{
	((QStyleOptionToolButton *)handle)->font = *(QFont *)font;
}

QStyleOptionToolButtonH QStyleOptionToolButton_Create()
{
	return (QStyleOptionToolButtonH) new QStyleOptionToolButton();
}

void QStyleOptionToolButton_Destroy(QStyleOptionToolButtonH handle)
{
	delete (QStyleOptionToolButton *)handle;
}

QStyleOptionToolButtonH QStyleOptionToolButton_Create2(const QStyleOptionToolButtonH other)
{
	return (QStyleOptionToolButtonH) new QStyleOptionToolButton(*(const QStyleOptionToolButton*)other);
}

bool QStyleOptionComboBox_editable(QStyleOptionComboBoxH handle)
{
	return (bool) ((QStyleOptionComboBox *)handle)->editable;
}

void QStyleOptionComboBox_setEditable(QStyleOptionComboBoxH handle, bool editable)
{
	((QStyleOptionComboBox *)handle)->editable = editable;
}

void QStyleOptionComboBox_popupRect(QStyleOptionComboBoxH handle, PRect retval)
{
	copyQRectToPRect(((QStyleOptionComboBox *)handle)->popupRect,retval);
}

void QStyleOptionComboBox_setPopupRect(QStyleOptionComboBoxH handle, PRect popupRect)
{
	copyPRectToQRect(popupRect,((QStyleOptionComboBox *)handle)->popupRect);
}

bool QStyleOptionComboBox_frame(QStyleOptionComboBoxH handle)
{
	return (bool) ((QStyleOptionComboBox *)handle)->frame;
}

void QStyleOptionComboBox_setFrame(QStyleOptionComboBoxH handle, bool frame)
{
	((QStyleOptionComboBox *)handle)->frame = frame;
}

void QStyleOptionComboBox_currentText(QStyleOptionComboBoxH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionComboBox *)handle)->currentText,retval);
}

void QStyleOptionComboBox_setCurrentText(QStyleOptionComboBoxH handle, PWideString currentText)
{
	copyPWideStringToQString(currentText,((QStyleOptionComboBox *)handle)->currentText);
}

void QStyleOptionComboBox_currentIcon(QStyleOptionComboBoxH handle, QIconH retval)
{
	 *(QIcon *)retval = ((QStyleOptionComboBox *)handle)->currentIcon;
}

void QStyleOptionComboBox_setCurrentIcon(QStyleOptionComboBoxH handle, QIconH currentIcon)
{
	((QStyleOptionComboBox *)handle)->currentIcon = *(QIcon *)currentIcon;
}

void QStyleOptionComboBox_iconSize(QStyleOptionComboBoxH handle, PSize retval)
{
	 *(QSize *)retval = ((QStyleOptionComboBox *)handle)->iconSize;
}

void QStyleOptionComboBox_setIconSize(QStyleOptionComboBoxH handle, PSize iconSize)
{
	((QStyleOptionComboBox *)handle)->iconSize = *(QSize *)iconSize;
}

QStyleOptionComboBoxH QStyleOptionComboBox_Create()
{
	return (QStyleOptionComboBoxH) new QStyleOptionComboBox();
}

void QStyleOptionComboBox_Destroy(QStyleOptionComboBoxH handle)
{
	delete (QStyleOptionComboBox *)handle;
}

QStyleOptionComboBoxH QStyleOptionComboBox_Create2(const QStyleOptionComboBoxH other)
{
	return (QStyleOptionComboBoxH) new QStyleOptionComboBox(*(const QStyleOptionComboBox*)other);
}

void QStyleOptionTitleBar_text(QStyleOptionTitleBarH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionTitleBar *)handle)->text,retval);
}

void QStyleOptionTitleBar_setText(QStyleOptionTitleBarH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionTitleBar *)handle)->text);
}

void QStyleOptionTitleBar_icon(QStyleOptionTitleBarH handle, QIconH retval)
{
	 *(QIcon *)retval = ((QStyleOptionTitleBar *)handle)->icon;
}

void QStyleOptionTitleBar_setIcon(QStyleOptionTitleBarH handle, QIconH icon)
{
	((QStyleOptionTitleBar *)handle)->icon = *(QIcon *)icon;
}

int QStyleOptionTitleBar_titleBarState(QStyleOptionTitleBarH handle)
{
	return (int) ((QStyleOptionTitleBar *)handle)->titleBarState;
}

void QStyleOptionTitleBar_setTitleBarState(QStyleOptionTitleBarH handle, int titleBarState)
{
	((QStyleOptionTitleBar *)handle)->titleBarState = titleBarState;
}

unsigned int QStyleOptionTitleBar_titleBarFlags(QStyleOptionTitleBarH handle)
{
	return (unsigned int) ((QStyleOptionTitleBar *)handle)->titleBarFlags;
}

void QStyleOptionTitleBar_setTitleBarFlags(QStyleOptionTitleBarH handle, unsigned int titleBarFlags)
{
	((QStyleOptionTitleBar *)handle)->titleBarFlags =(Qt::WindowFlags)titleBarFlags;
}

QStyleOptionTitleBarH QStyleOptionTitleBar_Create()
{
	return (QStyleOptionTitleBarH) new QStyleOptionTitleBar();
}

void QStyleOptionTitleBar_Destroy(QStyleOptionTitleBarH handle)
{
	delete (QStyleOptionTitleBar *)handle;
}

QStyleOptionTitleBarH QStyleOptionTitleBar_Create2(const QStyleOptionTitleBarH other)
{
	return (QStyleOptionTitleBarH) new QStyleOptionTitleBar(*(const QStyleOptionTitleBar*)other);
}

unsigned int QStyleOptionGroupBox_features(QStyleOptionGroupBoxH handle)
{
	return (unsigned int) ((QStyleOptionGroupBox *)handle)->features;
}

void QStyleOptionGroupBox_setFeatures(QStyleOptionGroupBoxH handle, unsigned int features)
{
	((QStyleOptionGroupBox *)handle)->features =(QStyleOptionFrame::FrameFeatures)features;
}

void QStyleOptionGroupBox_text(QStyleOptionGroupBoxH handle, PWideString retval)
{
	copyQStringToPWideString(((QStyleOptionGroupBox *)handle)->text,retval);
}

void QStyleOptionGroupBox_setText(QStyleOptionGroupBoxH handle, PWideString text)
{
	copyPWideStringToQString(text,((QStyleOptionGroupBox *)handle)->text);
}

unsigned int QStyleOptionGroupBox_textAlignment(QStyleOptionGroupBoxH handle)
{
	return (unsigned int) ((QStyleOptionGroupBox *)handle)->textAlignment;
}

void QStyleOptionGroupBox_setTextAlignment(QStyleOptionGroupBoxH handle, unsigned int textAlignment)
{
	((QStyleOptionGroupBox *)handle)->textAlignment =(Qt::Alignment)textAlignment;
}

void QStyleOptionGroupBox_textColor(QStyleOptionGroupBoxH handle, PQColor retval)
{
	 *(QColor *)retval = ((QStyleOptionGroupBox *)handle)->textColor;
}

void QStyleOptionGroupBox_setTextColor(QStyleOptionGroupBoxH handle, PQColor textColor)
{
	((QStyleOptionGroupBox *)handle)->textColor = *(QColor *)textColor;
}

int QStyleOptionGroupBox_lineWidth(QStyleOptionGroupBoxH handle)
{
	return (int) ((QStyleOptionGroupBox *)handle)->lineWidth;
}

void QStyleOptionGroupBox_setLineWidth(QStyleOptionGroupBoxH handle, int lineWidth)
{
	((QStyleOptionGroupBox *)handle)->lineWidth = lineWidth;
}

int QStyleOptionGroupBox_midLineWidth(QStyleOptionGroupBoxH handle)
{
	return (int) ((QStyleOptionGroupBox *)handle)->midLineWidth;
}

void QStyleOptionGroupBox_setMidLineWidth(QStyleOptionGroupBoxH handle, int midLineWidth)
{
	((QStyleOptionGroupBox *)handle)->midLineWidth = midLineWidth;
}

QStyleOptionGroupBoxH QStyleOptionGroupBox_Create()
{
	return (QStyleOptionGroupBoxH) new QStyleOptionGroupBox();
}

void QStyleOptionGroupBox_Destroy(QStyleOptionGroupBoxH handle)
{
	delete (QStyleOptionGroupBox *)handle;
}

QStyleOptionGroupBoxH QStyleOptionGroupBox_Create2(const QStyleOptionGroupBoxH other)
{
	return (QStyleOptionGroupBoxH) new QStyleOptionGroupBox(*(const QStyleOptionGroupBox*)other);
}

Qt::Corner QStyleOptionSizeGrip_corner(QStyleOptionSizeGripH handle)
{
	return (Qt::Corner) ((QStyleOptionSizeGrip *)handle)->corner;
}

void QStyleOptionSizeGrip_setCorner(QStyleOptionSizeGripH handle, Qt::Corner corner)
{
	((QStyleOptionSizeGrip *)handle)->corner = corner;
}

QStyleOptionSizeGripH QStyleOptionSizeGrip_Create()
{
	return (QStyleOptionSizeGripH) new QStyleOptionSizeGrip();
}

void QStyleOptionSizeGrip_Destroy(QStyleOptionSizeGripH handle)
{
	delete (QStyleOptionSizeGrip *)handle;
}

QStyleOptionSizeGripH QStyleOptionSizeGrip_Create2(const QStyleOptionSizeGripH other)
{
	return (QStyleOptionSizeGripH) new QStyleOptionSizeGrip(*(const QStyleOptionSizeGrip*)other);
}

void QStyleOptionGraphicsItem_exposedRect(QStyleOptionGraphicsItemH handle, QRectFH retval)
{
	 *(QRectF *)retval = ((QStyleOptionGraphicsItem *)handle)->exposedRect;
}

void QStyleOptionGraphicsItem_setExposedRect(QStyleOptionGraphicsItemH handle, QRectFH exposedRect)
{
	((QStyleOptionGraphicsItem *)handle)->exposedRect = *(QRectF *)exposedRect;
}

QStyleOptionGraphicsItemH QStyleOptionGraphicsItem_Create()
{
	return (QStyleOptionGraphicsItemH) new QStyleOptionGraphicsItem();
}

void QStyleOptionGraphicsItem_Destroy(QStyleOptionGraphicsItemH handle)
{
	delete (QStyleOptionGraphicsItem *)handle;
}

QStyleOptionGraphicsItemH QStyleOptionGraphicsItem_Create2(const QStyleOptionGraphicsItemH other)
{
	return (QStyleOptionGraphicsItemH) new QStyleOptionGraphicsItem(*(const QStyleOptionGraphicsItem*)other);
}

qreal QStyleOptionGraphicsItem_levelOfDetailFromTransform(const QTransformH worldTransform)
{
	return (qreal) QStyleOptionGraphicsItem::levelOfDetailFromTransform(*(const QTransform*)worldTransform);
}

int QStyleHintReturn_version(QStyleHintReturnH handle)
{
	return (int) ((QStyleHintReturn *)handle)->version;
}

void QStyleHintReturn_setVersion(QStyleHintReturnH handle, int version)
{
	((QStyleHintReturn *)handle)->version = version;
}

int QStyleHintReturn_type(QStyleHintReturnH handle)
{
	return (int) ((QStyleHintReturn *)handle)->type;
}

void QStyleHintReturn_setType(QStyleHintReturnH handle, int type)
{
	((QStyleHintReturn *)handle)->type = type;
}

QStyleHintReturnH QStyleHintReturn_Create(int version, int type)
{
	return (QStyleHintReturnH) new QStyleHintReturn(version, type);
}

void QStyleHintReturn_Destroy(QStyleHintReturnH handle)
{
	delete (QStyleHintReturn *)handle;
}

void QStyleHintReturnMask_region(QStyleHintReturnMaskH handle, QRegionH retval)
{
	 *(QRegion *)retval = ((QStyleHintReturnMask *)handle)->region;
}

void QStyleHintReturnMask_setRegion(QStyleHintReturnMaskH handle, QRegionH region)
{
	((QStyleHintReturnMask *)handle)->region = *(QRegion *)region;
}

QStyleHintReturnMaskH QStyleHintReturnMask_Create()
{
	return (QStyleHintReturnMaskH) new QStyleHintReturnMask();
}

void QStyleHintReturnMask_Destroy(QStyleHintReturnMaskH handle)
{
	delete (QStyleHintReturnMask *)handle;
}

void QStyleHintReturnVariant_variant(QStyleHintReturnVariantH handle, QVariantH retval)
{
	 *(QVariant *)retval = ((QStyleHintReturnVariant *)handle)->variant;
}

void QStyleHintReturnVariant_setVariant(QStyleHintReturnVariantH handle, QVariantH variant)
{
	((QStyleHintReturnVariant *)handle)->variant = *(QVariant *)variant;
}

QStyleHintReturnVariantH QStyleHintReturnVariant_Create()
{
	return (QStyleHintReturnVariantH) new QStyleHintReturnVariant();
}

void QStyleHintReturnVariant_Destroy(QStyleHintReturnVariantH handle)
{
	delete (QStyleHintReturnVariant *)handle;
}

