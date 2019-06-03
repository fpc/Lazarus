//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextformat_c.h"

QTextLengthH QTextLength_Create()
{
	return (QTextLengthH) new QTextLength();
}

void QTextLength_Destroy(QTextLengthH handle)
{
	delete (QTextLength *)handle;
}

QTextLengthH QTextLength_Create2(QTextLength::Type type, qreal value)
{
	return (QTextLengthH) new QTextLength(type, value);
}

QTextLength::Type QTextLength_type(QTextLengthH handle)
{
	return (QTextLength::Type) ((QTextLength *)handle)->type();
}

qreal QTextLength_value(QTextLengthH handle, qreal maximumLength)
{
	return (qreal) ((QTextLength *)handle)->value(maximumLength);
}

qreal QTextLength_rawValue(QTextLengthH handle)
{
	return (qreal) ((QTextLength *)handle)->rawValue();
}

QTextFormatH QTextFormat_Create()
{
	return (QTextFormatH) new QTextFormat();
}

void QTextFormat_Destroy(QTextFormatH handle)
{
	delete (QTextFormat *)handle;
}

QTextFormatH QTextFormat_Create2(int type)
{
	return (QTextFormatH) new QTextFormat(type);
}

QTextFormatH QTextFormat_Create3(const QTextFormatH rhs)
{
	return (QTextFormatH) new QTextFormat(*(const QTextFormat*)rhs);
}

void QTextFormat_swap(QTextFormatH handle, QTextFormatH other)
{
	((QTextFormat *)handle)->swap(*(QTextFormat*)other);
}

void QTextFormat_merge(QTextFormatH handle, const QTextFormatH other)
{
	((QTextFormat *)handle)->merge(*(const QTextFormat*)other);
}

bool QTextFormat_isEmpty(QTextFormatH handle)
{
	return (bool) ((QTextFormat *)handle)->isEmpty();
}

bool QTextFormat_isValid(QTextFormatH handle)
{
	return (bool) ((QTextFormat *)handle)->isValid();
}

int QTextFormat_type(QTextFormatH handle)
{
	return (int) ((QTextFormat *)handle)->type();
}

int QTextFormat_objectIndex(QTextFormatH handle)
{
	return (int) ((QTextFormat *)handle)->objectIndex();
}

void QTextFormat_setObjectIndex(QTextFormatH handle, int object)
{
	((QTextFormat *)handle)->setObjectIndex(object);
}

void QTextFormat_property(QTextFormatH handle, QVariantH retval, int propertyId)
{
	*(QVariant *)retval = ((QTextFormat *)handle)->property(propertyId);
}

void QTextFormat_setProperty(QTextFormatH handle, int propertyId, const QVariantH value)
{
	((QTextFormat *)handle)->setProperty(propertyId, *(const QVariant*)value);
}

void QTextFormat_clearProperty(QTextFormatH handle, int propertyId)
{
	((QTextFormat *)handle)->clearProperty(propertyId);
}

bool QTextFormat_hasProperty(QTextFormatH handle, int propertyId)
{
	return (bool) ((QTextFormat *)handle)->hasProperty(propertyId);
}

bool QTextFormat_boolProperty(QTextFormatH handle, int propertyId)
{
	return (bool) ((QTextFormat *)handle)->boolProperty(propertyId);
}

int QTextFormat_intProperty(QTextFormatH handle, int propertyId)
{
	return (int) ((QTextFormat *)handle)->intProperty(propertyId);
}

qreal QTextFormat_doubleProperty(QTextFormatH handle, int propertyId)
{
	return (qreal) ((QTextFormat *)handle)->doubleProperty(propertyId);
}

void QTextFormat_stringProperty(QTextFormatH handle, PWideString retval, int propertyId)
{
	QString t_retval;
	t_retval = ((QTextFormat *)handle)->stringProperty(propertyId);
	copyQStringToPWideString(t_retval, retval);
}

void QTextFormat_colorProperty(QTextFormatH handle, PQColor retval, int propertyId)
{
	*(QColor *)retval = ((QTextFormat *)handle)->colorProperty(propertyId);
}

void QTextFormat_penProperty(QTextFormatH handle, QPenH retval, int propertyId)
{
	*(QPen *)retval = ((QTextFormat *)handle)->penProperty(propertyId);
}

void QTextFormat_brushProperty(QTextFormatH handle, QBrushH retval, int propertyId)
{
	*(QBrush *)retval = ((QTextFormat *)handle)->brushProperty(propertyId);
}

void QTextFormat_lengthProperty(QTextFormatH handle, QTextLengthH retval, int propertyId)
{
	*(QTextLength *)retval = ((QTextFormat *)handle)->lengthProperty(propertyId);
}

int QTextFormat_propertyCount(QTextFormatH handle)
{
	return (int) ((QTextFormat *)handle)->propertyCount();
}

void QTextFormat_setObjectType(QTextFormatH handle, int type)
{
	((QTextFormat *)handle)->setObjectType(type);
}

int QTextFormat_objectType(QTextFormatH handle)
{
	return (int) ((QTextFormat *)handle)->objectType();
}

bool QTextFormat_isCharFormat(QTextFormatH handle)
{
	return (bool) ((QTextFormat *)handle)->isCharFormat();
}

bool QTextFormat_isBlockFormat(QTextFormatH handle)
{
	return (bool) ((QTextFormat *)handle)->isBlockFormat();
}

bool QTextFormat_isListFormat(QTextFormatH handle)
{
	return (bool) ((QTextFormat *)handle)->isListFormat();
}

bool QTextFormat_isFrameFormat(QTextFormatH handle)
{
	return (bool) ((QTextFormat *)handle)->isFrameFormat();
}

bool QTextFormat_isImageFormat(QTextFormatH handle)
{
	return (bool) ((QTextFormat *)handle)->isImageFormat();
}

bool QTextFormat_isTableFormat(QTextFormatH handle)
{
	return (bool) ((QTextFormat *)handle)->isTableFormat();
}

bool QTextFormat_isTableCellFormat(QTextFormatH handle)
{
	return (bool) ((QTextFormat *)handle)->isTableCellFormat();
}

void QTextFormat_toBlockFormat(QTextFormatH handle, QTextBlockFormatH retval)
{
	*(QTextBlockFormat *)retval = ((QTextFormat *)handle)->toBlockFormat();
}

void QTextFormat_toCharFormat(QTextFormatH handle, QTextCharFormatH retval)
{
	*(QTextCharFormat *)retval = ((QTextFormat *)handle)->toCharFormat();
}

void QTextFormat_toListFormat(QTextFormatH handle, QTextListFormatH retval)
{
	*(QTextListFormat *)retval = ((QTextFormat *)handle)->toListFormat();
}

void QTextFormat_toTableFormat(QTextFormatH handle, QTextTableFormatH retval)
{
	*(QTextTableFormat *)retval = ((QTextFormat *)handle)->toTableFormat();
}

void QTextFormat_toFrameFormat(QTextFormatH handle, QTextFrameFormatH retval)
{
	*(QTextFrameFormat *)retval = ((QTextFormat *)handle)->toFrameFormat();
}

void QTextFormat_toImageFormat(QTextFormatH handle, QTextImageFormatH retval)
{
	*(QTextImageFormat *)retval = ((QTextFormat *)handle)->toImageFormat();
}

void QTextFormat_toTableCellFormat(QTextFormatH handle, QTextTableCellFormatH retval)
{
	*(QTextTableCellFormat *)retval = ((QTextFormat *)handle)->toTableCellFormat();
}

void QTextFormat_setLayoutDirection(QTextFormatH handle, Qt::LayoutDirection direction)
{
	((QTextFormat *)handle)->setLayoutDirection(direction);
}

Qt::LayoutDirection QTextFormat_layoutDirection(QTextFormatH handle)
{
	return (Qt::LayoutDirection) ((QTextFormat *)handle)->layoutDirection();
}

void QTextFormat_setBackground(QTextFormatH handle, const QBrushH brush)
{
	((QTextFormat *)handle)->setBackground(*(const QBrush*)brush);
}

void QTextFormat_background(QTextFormatH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QTextFormat *)handle)->background();
}

void QTextFormat_clearBackground(QTextFormatH handle)
{
	((QTextFormat *)handle)->clearBackground();
}

void QTextFormat_setForeground(QTextFormatH handle, const QBrushH brush)
{
	((QTextFormat *)handle)->setForeground(*(const QBrush*)brush);
}

void QTextFormat_foreground(QTextFormatH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QTextFormat *)handle)->foreground();
}

void QTextFormat_clearForeground(QTextFormatH handle)
{
	((QTextFormat *)handle)->clearForeground();
}

QTextCharFormatH QTextCharFormat_Create()
{
	return (QTextCharFormatH) new QTextCharFormat();
}

void QTextCharFormat_Destroy(QTextCharFormatH handle)
{
	delete (QTextCharFormat *)handle;
}

bool QTextCharFormat_isValid(QTextCharFormatH handle)
{
	return (bool) ((QTextCharFormat *)handle)->isValid();
}

void QTextCharFormat_setFont(QTextCharFormatH handle, const QFontH font)
{
	((QTextCharFormat *)handle)->setFont(*(const QFont*)font);
}

void QTextCharFormat_font(QTextCharFormatH handle, QFontH retval)
{
	*(QFont *)retval = ((QTextCharFormat *)handle)->font();
}

void QTextCharFormat_setFontFamily(QTextCharFormatH handle, PWideString family)
{
	QString t_family;
	copyPWideStringToQString(family, t_family);
	((QTextCharFormat *)handle)->setFontFamily(t_family);
}

void QTextCharFormat_fontFamily(QTextCharFormatH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextCharFormat *)handle)->fontFamily();
	copyQStringToPWideString(t_retval, retval);
}

void QTextCharFormat_setFontPointSize(QTextCharFormatH handle, qreal size)
{
	((QTextCharFormat *)handle)->setFontPointSize(size);
}

qreal QTextCharFormat_fontPointSize(QTextCharFormatH handle)
{
	return (qreal) ((QTextCharFormat *)handle)->fontPointSize();
}

void QTextCharFormat_setFontWeight(QTextCharFormatH handle, int weight)
{
	((QTextCharFormat *)handle)->setFontWeight(weight);
}

int QTextCharFormat_fontWeight(QTextCharFormatH handle)
{
	return (int) ((QTextCharFormat *)handle)->fontWeight();
}

void QTextCharFormat_setFontItalic(QTextCharFormatH handle, bool italic)
{
	((QTextCharFormat *)handle)->setFontItalic(italic);
}

bool QTextCharFormat_fontItalic(QTextCharFormatH handle)
{
	return (bool) ((QTextCharFormat *)handle)->fontItalic();
}

void QTextCharFormat_setFontCapitalization(QTextCharFormatH handle, QFont::Capitalization capitalization)
{
	((QTextCharFormat *)handle)->setFontCapitalization(capitalization);
}

QFont::Capitalization QTextCharFormat_fontCapitalization(QTextCharFormatH handle)
{
	return (QFont::Capitalization) ((QTextCharFormat *)handle)->fontCapitalization();
}

void QTextCharFormat_setFontLetterSpacingType(QTextCharFormatH handle, QFont::SpacingType letterSpacingType)
{
	((QTextCharFormat *)handle)->setFontLetterSpacingType(letterSpacingType);
}

QFont::SpacingType QTextCharFormat_fontLetterSpacingType(QTextCharFormatH handle)
{
	return (QFont::SpacingType) ((QTextCharFormat *)handle)->fontLetterSpacingType();
}

void QTextCharFormat_setFontLetterSpacing(QTextCharFormatH handle, qreal spacing)
{
	((QTextCharFormat *)handle)->setFontLetterSpacing(spacing);
}

qreal QTextCharFormat_fontLetterSpacing(QTextCharFormatH handle)
{
	return (qreal) ((QTextCharFormat *)handle)->fontLetterSpacing();
}

void QTextCharFormat_setFontWordSpacing(QTextCharFormatH handle, qreal spacing)
{
	((QTextCharFormat *)handle)->setFontWordSpacing(spacing);
}

qreal QTextCharFormat_fontWordSpacing(QTextCharFormatH handle)
{
	return (qreal) ((QTextCharFormat *)handle)->fontWordSpacing();
}

void QTextCharFormat_setFontUnderline(QTextCharFormatH handle, bool underline)
{
	((QTextCharFormat *)handle)->setFontUnderline(underline);
}

bool QTextCharFormat_fontUnderline(QTextCharFormatH handle)
{
	return (bool) ((QTextCharFormat *)handle)->fontUnderline();
}

void QTextCharFormat_setFontOverline(QTextCharFormatH handle, bool overline)
{
	((QTextCharFormat *)handle)->setFontOverline(overline);
}

bool QTextCharFormat_fontOverline(QTextCharFormatH handle)
{
	return (bool) ((QTextCharFormat *)handle)->fontOverline();
}

void QTextCharFormat_setFontStrikeOut(QTextCharFormatH handle, bool strikeOut)
{
	((QTextCharFormat *)handle)->setFontStrikeOut(strikeOut);
}

bool QTextCharFormat_fontStrikeOut(QTextCharFormatH handle)
{
	return (bool) ((QTextCharFormat *)handle)->fontStrikeOut();
}

void QTextCharFormat_setUnderlineColor(QTextCharFormatH handle, const QColorH color)
{
	((QTextCharFormat *)handle)->setUnderlineColor(*(const QColor*)color);
}

void QTextCharFormat_underlineColor(QTextCharFormatH handle, PQColor retval)
{
	*(QColor *)retval = ((QTextCharFormat *)handle)->underlineColor();
}

void QTextCharFormat_setFontFixedPitch(QTextCharFormatH handle, bool fixedPitch)
{
	((QTextCharFormat *)handle)->setFontFixedPitch(fixedPitch);
}

bool QTextCharFormat_fontFixedPitch(QTextCharFormatH handle)
{
	return (bool) ((QTextCharFormat *)handle)->fontFixedPitch();
}

void QTextCharFormat_setFontStretch(QTextCharFormatH handle, int factor)
{
	((QTextCharFormat *)handle)->setFontStretch(factor);
}

int QTextCharFormat_fontStretch(QTextCharFormatH handle)
{
	return (int) ((QTextCharFormat *)handle)->fontStretch();
}

void QTextCharFormat_setFontStyleHint(QTextCharFormatH handle, QFont::StyleHint hint, QFont::StyleStrategy strategy)
{
	((QTextCharFormat *)handle)->setFontStyleHint(hint, strategy);
}

void QTextCharFormat_setFontStyleStrategy(QTextCharFormatH handle, QFont::StyleStrategy strategy)
{
	((QTextCharFormat *)handle)->setFontStyleStrategy(strategy);
}

QFont::StyleHint QTextCharFormat_fontStyleHint(QTextCharFormatH handle)
{
	return (QFont::StyleHint) ((QTextCharFormat *)handle)->fontStyleHint();
}

QFont::StyleStrategy QTextCharFormat_fontStyleStrategy(QTextCharFormatH handle)
{
	return (QFont::StyleStrategy) ((QTextCharFormat *)handle)->fontStyleStrategy();
}

void QTextCharFormat_setFontHintingPreference(QTextCharFormatH handle, QFont::HintingPreference hintingPreference)
{
	((QTextCharFormat *)handle)->setFontHintingPreference(hintingPreference);
}

QFont::HintingPreference QTextCharFormat_fontHintingPreference(QTextCharFormatH handle)
{
	return (QFont::HintingPreference) ((QTextCharFormat *)handle)->fontHintingPreference();
}

void QTextCharFormat_setFontKerning(QTextCharFormatH handle, bool enable)
{
	((QTextCharFormat *)handle)->setFontKerning(enable);
}

bool QTextCharFormat_fontKerning(QTextCharFormatH handle)
{
	return (bool) ((QTextCharFormat *)handle)->fontKerning();
}

void QTextCharFormat_setUnderlineStyle(QTextCharFormatH handle, QTextCharFormat::UnderlineStyle style)
{
	((QTextCharFormat *)handle)->setUnderlineStyle(style);
}

QTextCharFormat::UnderlineStyle QTextCharFormat_underlineStyle(QTextCharFormatH handle)
{
	return (QTextCharFormat::UnderlineStyle) ((QTextCharFormat *)handle)->underlineStyle();
}

void QTextCharFormat_setVerticalAlignment(QTextCharFormatH handle, QTextCharFormat::VerticalAlignment alignment)
{
	((QTextCharFormat *)handle)->setVerticalAlignment(alignment);
}

QTextCharFormat::VerticalAlignment QTextCharFormat_verticalAlignment(QTextCharFormatH handle)
{
	return (QTextCharFormat::VerticalAlignment) ((QTextCharFormat *)handle)->verticalAlignment();
}

void QTextCharFormat_setTextOutline(QTextCharFormatH handle, const QPenH pen)
{
	((QTextCharFormat *)handle)->setTextOutline(*(const QPen*)pen);
}

void QTextCharFormat_textOutline(QTextCharFormatH handle, QPenH retval)
{
	*(QPen *)retval = ((QTextCharFormat *)handle)->textOutline();
}

void QTextCharFormat_setToolTip(QTextCharFormatH handle, PWideString tip)
{
	QString t_tip;
	copyPWideStringToQString(tip, t_tip);
	((QTextCharFormat *)handle)->setToolTip(t_tip);
}

void QTextCharFormat_toolTip(QTextCharFormatH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextCharFormat *)handle)->toolTip();
	copyQStringToPWideString(t_retval, retval);
}

void QTextCharFormat_setAnchor(QTextCharFormatH handle, bool anchor)
{
	((QTextCharFormat *)handle)->setAnchor(anchor);
}

bool QTextCharFormat_isAnchor(QTextCharFormatH handle)
{
	return (bool) ((QTextCharFormat *)handle)->isAnchor();
}

void QTextCharFormat_setAnchorHref(QTextCharFormatH handle, PWideString value)
{
	QString t_value;
	copyPWideStringToQString(value, t_value);
	((QTextCharFormat *)handle)->setAnchorHref(t_value);
}

void QTextCharFormat_anchorHref(QTextCharFormatH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextCharFormat *)handle)->anchorHref();
	copyQStringToPWideString(t_retval, retval);
}

void QTextCharFormat_setAnchorNames(QTextCharFormatH handle, const QStringListH names)
{
	((QTextCharFormat *)handle)->setAnchorNames(*(const QStringList*)names);
}

void QTextCharFormat_anchorNames(QTextCharFormatH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QTextCharFormat *)handle)->anchorNames();
}

void QTextCharFormat_setTableCellRowSpan(QTextCharFormatH handle, int tableCellRowSpan)
{
	((QTextCharFormat *)handle)->setTableCellRowSpan(tableCellRowSpan);
}

int QTextCharFormat_tableCellRowSpan(QTextCharFormatH handle)
{
	return (int) ((QTextCharFormat *)handle)->tableCellRowSpan();
}

void QTextCharFormat_setTableCellColumnSpan(QTextCharFormatH handle, int tableCellColumnSpan)
{
	((QTextCharFormat *)handle)->setTableCellColumnSpan(tableCellColumnSpan);
}

int QTextCharFormat_tableCellColumnSpan(QTextCharFormatH handle)
{
	return (int) ((QTextCharFormat *)handle)->tableCellColumnSpan();
}

QTextBlockFormatH QTextBlockFormat_Create()
{
	return (QTextBlockFormatH) new QTextBlockFormat();
}

void QTextBlockFormat_Destroy(QTextBlockFormatH handle)
{
	delete (QTextBlockFormat *)handle;
}

bool QTextBlockFormat_isValid(QTextBlockFormatH handle)
{
	return (bool) ((QTextBlockFormat *)handle)->isValid();
}

void QTextBlockFormat_setAlignment(QTextBlockFormatH handle, unsigned int alignment)
{
	((QTextBlockFormat *)handle)->setAlignment((Qt::Alignment)alignment);
}

unsigned int QTextBlockFormat_alignment(QTextBlockFormatH handle)
{
	return (unsigned int) ((QTextBlockFormat *)handle)->alignment();
}

void QTextBlockFormat_setTopMargin(QTextBlockFormatH handle, qreal margin)
{
	((QTextBlockFormat *)handle)->setTopMargin(margin);
}

qreal QTextBlockFormat_topMargin(QTextBlockFormatH handle)
{
	return (qreal) ((QTextBlockFormat *)handle)->topMargin();
}

void QTextBlockFormat_setBottomMargin(QTextBlockFormatH handle, qreal margin)
{
	((QTextBlockFormat *)handle)->setBottomMargin(margin);
}

qreal QTextBlockFormat_bottomMargin(QTextBlockFormatH handle)
{
	return (qreal) ((QTextBlockFormat *)handle)->bottomMargin();
}

void QTextBlockFormat_setLeftMargin(QTextBlockFormatH handle, qreal margin)
{
	((QTextBlockFormat *)handle)->setLeftMargin(margin);
}

qreal QTextBlockFormat_leftMargin(QTextBlockFormatH handle)
{
	return (qreal) ((QTextBlockFormat *)handle)->leftMargin();
}

void QTextBlockFormat_setRightMargin(QTextBlockFormatH handle, qreal margin)
{
	((QTextBlockFormat *)handle)->setRightMargin(margin);
}

qreal QTextBlockFormat_rightMargin(QTextBlockFormatH handle)
{
	return (qreal) ((QTextBlockFormat *)handle)->rightMargin();
}

void QTextBlockFormat_setTextIndent(QTextBlockFormatH handle, qreal aindent)
{
	((QTextBlockFormat *)handle)->setTextIndent(aindent);
}

qreal QTextBlockFormat_textIndent(QTextBlockFormatH handle)
{
	return (qreal) ((QTextBlockFormat *)handle)->textIndent();
}

void QTextBlockFormat_setIndent(QTextBlockFormatH handle, int indent)
{
	((QTextBlockFormat *)handle)->setIndent(indent);
}

int QTextBlockFormat_indent(QTextBlockFormatH handle)
{
	return (int) ((QTextBlockFormat *)handle)->indent();
}

void QTextBlockFormat_setLineHeight(QTextBlockFormatH handle, qreal height, int heightType)
{
	((QTextBlockFormat *)handle)->setLineHeight(height, heightType);
}

qreal QTextBlockFormat_lineHeight(QTextBlockFormatH handle, qreal scriptLineHeight, qreal scaling)
{
	return (qreal) ((QTextBlockFormat *)handle)->lineHeight(scriptLineHeight, scaling);
}

qreal QTextBlockFormat_lineHeight2(QTextBlockFormatH handle)
{
	return (qreal) ((QTextBlockFormat *)handle)->lineHeight();
}

int QTextBlockFormat_lineHeightType(QTextBlockFormatH handle)
{
	return (int) ((QTextBlockFormat *)handle)->lineHeightType();
}

void QTextBlockFormat_setNonBreakableLines(QTextBlockFormatH handle, bool b)
{
	((QTextBlockFormat *)handle)->setNonBreakableLines(b);
}

bool QTextBlockFormat_nonBreakableLines(QTextBlockFormatH handle)
{
	return (bool) ((QTextBlockFormat *)handle)->nonBreakableLines();
}

void QTextBlockFormat_setPageBreakPolicy(QTextBlockFormatH handle, unsigned int flags)
{
	((QTextBlockFormat *)handle)->setPageBreakPolicy((QTextFormat::PageBreakFlags)flags);
}

unsigned int QTextBlockFormat_pageBreakPolicy(QTextBlockFormatH handle)
{
	return (unsigned int) ((QTextBlockFormat *)handle)->pageBreakPolicy();
}

QTextListFormatH QTextListFormat_Create()
{
	return (QTextListFormatH) new QTextListFormat();
}

void QTextListFormat_Destroy(QTextListFormatH handle)
{
	delete (QTextListFormat *)handle;
}

bool QTextListFormat_isValid(QTextListFormatH handle)
{
	return (bool) ((QTextListFormat *)handle)->isValid();
}

void QTextListFormat_setStyle(QTextListFormatH handle, QTextListFormat::Style style)
{
	((QTextListFormat *)handle)->setStyle(style);
}

QTextListFormat::Style QTextListFormat_style(QTextListFormatH handle)
{
	return (QTextListFormat::Style) ((QTextListFormat *)handle)->style();
}

void QTextListFormat_setIndent(QTextListFormatH handle, int indent)
{
	((QTextListFormat *)handle)->setIndent(indent);
}

int QTextListFormat_indent(QTextListFormatH handle)
{
	return (int) ((QTextListFormat *)handle)->indent();
}

void QTextListFormat_setNumberPrefix(QTextListFormatH handle, PWideString numberPrefix)
{
	QString t_numberPrefix;
	copyPWideStringToQString(numberPrefix, t_numberPrefix);
	((QTextListFormat *)handle)->setNumberPrefix(t_numberPrefix);
}

void QTextListFormat_numberPrefix(QTextListFormatH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextListFormat *)handle)->numberPrefix();
	copyQStringToPWideString(t_retval, retval);
}

void QTextListFormat_setNumberSuffix(QTextListFormatH handle, PWideString numberSuffix)
{
	QString t_numberSuffix;
	copyPWideStringToQString(numberSuffix, t_numberSuffix);
	((QTextListFormat *)handle)->setNumberSuffix(t_numberSuffix);
}

void QTextListFormat_numberSuffix(QTextListFormatH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextListFormat *)handle)->numberSuffix();
	copyQStringToPWideString(t_retval, retval);
}

QTextImageFormatH QTextImageFormat_Create()
{
	return (QTextImageFormatH) new QTextImageFormat();
}

void QTextImageFormat_Destroy(QTextImageFormatH handle)
{
	delete (QTextImageFormat *)handle;
}

bool QTextImageFormat_isValid(QTextImageFormatH handle)
{
	return (bool) ((QTextImageFormat *)handle)->isValid();
}

void QTextImageFormat_setName(QTextImageFormatH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QTextImageFormat *)handle)->setName(t_name);
}

void QTextImageFormat_name(QTextImageFormatH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextImageFormat *)handle)->name();
	copyQStringToPWideString(t_retval, retval);
}

void QTextImageFormat_setWidth(QTextImageFormatH handle, qreal width)
{
	((QTextImageFormat *)handle)->setWidth(width);
}

qreal QTextImageFormat_width(QTextImageFormatH handle)
{
	return (qreal) ((QTextImageFormat *)handle)->width();
}

void QTextImageFormat_setHeight(QTextImageFormatH handle, qreal height)
{
	((QTextImageFormat *)handle)->setHeight(height);
}

qreal QTextImageFormat_height(QTextImageFormatH handle)
{
	return (qreal) ((QTextImageFormat *)handle)->height();
}

QTextFrameFormatH QTextFrameFormat_Create()
{
	return (QTextFrameFormatH) new QTextFrameFormat();
}

void QTextFrameFormat_Destroy(QTextFrameFormatH handle)
{
	delete (QTextFrameFormat *)handle;
}

bool QTextFrameFormat_isValid(QTextFrameFormatH handle)
{
	return (bool) ((QTextFrameFormat *)handle)->isValid();
}

void QTextFrameFormat_setPosition(QTextFrameFormatH handle, QTextFrameFormat::Position f)
{
	((QTextFrameFormat *)handle)->setPosition(f);
}

QTextFrameFormat::Position QTextFrameFormat_position(QTextFrameFormatH handle)
{
	return (QTextFrameFormat::Position) ((QTextFrameFormat *)handle)->position();
}

void QTextFrameFormat_setBorder(QTextFrameFormatH handle, qreal border)
{
	((QTextFrameFormat *)handle)->setBorder(border);
}

qreal QTextFrameFormat_border(QTextFrameFormatH handle)
{
	return (qreal) ((QTextFrameFormat *)handle)->border();
}

void QTextFrameFormat_setBorderBrush(QTextFrameFormatH handle, const QBrushH brush)
{
	((QTextFrameFormat *)handle)->setBorderBrush(*(const QBrush*)brush);
}

void QTextFrameFormat_borderBrush(QTextFrameFormatH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QTextFrameFormat *)handle)->borderBrush();
}

void QTextFrameFormat_setBorderStyle(QTextFrameFormatH handle, QTextFrameFormat::BorderStyle style)
{
	((QTextFrameFormat *)handle)->setBorderStyle(style);
}

QTextFrameFormat::BorderStyle QTextFrameFormat_borderStyle(QTextFrameFormatH handle)
{
	return (QTextFrameFormat::BorderStyle) ((QTextFrameFormat *)handle)->borderStyle();
}

void QTextFrameFormat_setMargin(QTextFrameFormatH handle, qreal margin)
{
	((QTextFrameFormat *)handle)->setMargin(margin);
}

qreal QTextFrameFormat_margin(QTextFrameFormatH handle)
{
	return (qreal) ((QTextFrameFormat *)handle)->margin();
}

void QTextFrameFormat_setTopMargin(QTextFrameFormatH handle, qreal margin)
{
	((QTextFrameFormat *)handle)->setTopMargin(margin);
}

qreal QTextFrameFormat_topMargin(QTextFrameFormatH handle)
{
	return (qreal) ((QTextFrameFormat *)handle)->topMargin();
}

void QTextFrameFormat_setBottomMargin(QTextFrameFormatH handle, qreal margin)
{
	((QTextFrameFormat *)handle)->setBottomMargin(margin);
}

qreal QTextFrameFormat_bottomMargin(QTextFrameFormatH handle)
{
	return (qreal) ((QTextFrameFormat *)handle)->bottomMargin();
}

void QTextFrameFormat_setLeftMargin(QTextFrameFormatH handle, qreal margin)
{
	((QTextFrameFormat *)handle)->setLeftMargin(margin);
}

qreal QTextFrameFormat_leftMargin(QTextFrameFormatH handle)
{
	return (qreal) ((QTextFrameFormat *)handle)->leftMargin();
}

void QTextFrameFormat_setRightMargin(QTextFrameFormatH handle, qreal margin)
{
	((QTextFrameFormat *)handle)->setRightMargin(margin);
}

qreal QTextFrameFormat_rightMargin(QTextFrameFormatH handle)
{
	return (qreal) ((QTextFrameFormat *)handle)->rightMargin();
}

void QTextFrameFormat_setPadding(QTextFrameFormatH handle, qreal padding)
{
	((QTextFrameFormat *)handle)->setPadding(padding);
}

qreal QTextFrameFormat_padding(QTextFrameFormatH handle)
{
	return (qreal) ((QTextFrameFormat *)handle)->padding();
}

void QTextFrameFormat_setWidth(QTextFrameFormatH handle, qreal width)
{
	((QTextFrameFormat *)handle)->setWidth(width);
}

void QTextFrameFormat_setWidth2(QTextFrameFormatH handle, const QTextLengthH length)
{
	((QTextFrameFormat *)handle)->setWidth(*(const QTextLength*)length);
}

void QTextFrameFormat_width(QTextFrameFormatH handle, QTextLengthH retval)
{
	*(QTextLength *)retval = ((QTextFrameFormat *)handle)->width();
}

void QTextFrameFormat_setHeight(QTextFrameFormatH handle, qreal height)
{
	((QTextFrameFormat *)handle)->setHeight(height);
}

void QTextFrameFormat_setHeight2(QTextFrameFormatH handle, const QTextLengthH height)
{
	((QTextFrameFormat *)handle)->setHeight(*(const QTextLength*)height);
}

void QTextFrameFormat_height(QTextFrameFormatH handle, QTextLengthH retval)
{
	*(QTextLength *)retval = ((QTextFrameFormat *)handle)->height();
}

void QTextFrameFormat_setPageBreakPolicy(QTextFrameFormatH handle, unsigned int flags)
{
	((QTextFrameFormat *)handle)->setPageBreakPolicy((QTextFormat::PageBreakFlags)flags);
}

unsigned int QTextFrameFormat_pageBreakPolicy(QTextFrameFormatH handle)
{
	return (unsigned int) ((QTextFrameFormat *)handle)->pageBreakPolicy();
}

QTextTableFormatH QTextTableFormat_Create()
{
	return (QTextTableFormatH) new QTextTableFormat();
}

void QTextTableFormat_Destroy(QTextTableFormatH handle)
{
	delete (QTextTableFormat *)handle;
}

bool QTextTableFormat_isValid(QTextTableFormatH handle)
{
	return (bool) ((QTextTableFormat *)handle)->isValid();
}

int QTextTableFormat_columns(QTextTableFormatH handle)
{
	return (int) ((QTextTableFormat *)handle)->columns();
}

void QTextTableFormat_setColumns(QTextTableFormatH handle, int columns)
{
	((QTextTableFormat *)handle)->setColumns(columns);
}

void QTextTableFormat_clearColumnWidthConstraints(QTextTableFormatH handle)
{
	((QTextTableFormat *)handle)->clearColumnWidthConstraints();
}

qreal QTextTableFormat_cellSpacing(QTextTableFormatH handle)
{
	return (qreal) ((QTextTableFormat *)handle)->cellSpacing();
}

void QTextTableFormat_setCellSpacing(QTextTableFormatH handle, qreal spacing)
{
	((QTextTableFormat *)handle)->setCellSpacing(spacing);
}

qreal QTextTableFormat_cellPadding(QTextTableFormatH handle)
{
	return (qreal) ((QTextTableFormat *)handle)->cellPadding();
}

void QTextTableFormat_setCellPadding(QTextTableFormatH handle, qreal padding)
{
	((QTextTableFormat *)handle)->setCellPadding(padding);
}

void QTextTableFormat_setAlignment(QTextTableFormatH handle, unsigned int alignment)
{
	((QTextTableFormat *)handle)->setAlignment((Qt::Alignment)alignment);
}

unsigned int QTextTableFormat_alignment(QTextTableFormatH handle)
{
	return (unsigned int) ((QTextTableFormat *)handle)->alignment();
}

void QTextTableFormat_setHeaderRowCount(QTextTableFormatH handle, int count)
{
	((QTextTableFormat *)handle)->setHeaderRowCount(count);
}

int QTextTableFormat_headerRowCount(QTextTableFormatH handle)
{
	return (int) ((QTextTableFormat *)handle)->headerRowCount();
}

QTextTableCellFormatH QTextTableCellFormat_Create()
{
	return (QTextTableCellFormatH) new QTextTableCellFormat();
}

void QTextTableCellFormat_Destroy(QTextTableCellFormatH handle)
{
	delete (QTextTableCellFormat *)handle;
}

bool QTextTableCellFormat_isValid(QTextTableCellFormatH handle)
{
	return (bool) ((QTextTableCellFormat *)handle)->isValid();
}

void QTextTableCellFormat_setTopPadding(QTextTableCellFormatH handle, qreal padding)
{
	((QTextTableCellFormat *)handle)->setTopPadding(padding);
}

qreal QTextTableCellFormat_topPadding(QTextTableCellFormatH handle)
{
	return (qreal) ((QTextTableCellFormat *)handle)->topPadding();
}

void QTextTableCellFormat_setBottomPadding(QTextTableCellFormatH handle, qreal padding)
{
	((QTextTableCellFormat *)handle)->setBottomPadding(padding);
}

qreal QTextTableCellFormat_bottomPadding(QTextTableCellFormatH handle)
{
	return (qreal) ((QTextTableCellFormat *)handle)->bottomPadding();
}

void QTextTableCellFormat_setLeftPadding(QTextTableCellFormatH handle, qreal padding)
{
	((QTextTableCellFormat *)handle)->setLeftPadding(padding);
}

qreal QTextTableCellFormat_leftPadding(QTextTableCellFormatH handle)
{
	return (qreal) ((QTextTableCellFormat *)handle)->leftPadding();
}

void QTextTableCellFormat_setRightPadding(QTextTableCellFormatH handle, qreal padding)
{
	((QTextTableCellFormat *)handle)->setRightPadding(padding);
}

qreal QTextTableCellFormat_rightPadding(QTextTableCellFormatH handle)
{
	return (qreal) ((QTextTableCellFormat *)handle)->rightPadding();
}

void QTextTableCellFormat_setPadding(QTextTableCellFormatH handle, qreal padding)
{
	((QTextTableCellFormat *)handle)->setPadding(padding);
}

