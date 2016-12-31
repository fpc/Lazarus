#ifndef QLCLITEMDELEGATE_H
#define QLCLITEMDELEGATE_H

#include <QItemDelegate>
#include "pascalbind.h"

class QLCLItemDelegate : public QItemDelegate {

public:

  //==================================================================================== 
  QLCLItemDelegate(QObject *parent = 0) : QItemDelegate(parent) {
    sizeHintOverride.func = NULL;
    paintOverride.func = NULL;
    createEditorOverride.func = NULL;
    setEditorDataOverride.func = NULL;
    setModelDataOverride.func = NULL;
    updateEditorGeometryOverride.func = NULL;
    editorEventOverride.func = NULL;
  };

  //==================================================================================== 
  void override_sizeHint(const QOverrideHook hook) {
    sizeHintOverride = hook; 
  }

  //==================================================================================== 
  void override_paint(const QOverrideHook hook) {
    paintOverride = hook; 
  }

  void override_createEditor(const QOverrideHook hook) {
    createEditorOverride = hook; 
  }

  void override_setEditorData(const QOverrideHook hook) {
    setEditorDataOverride = hook; 
  }

  void override_setModelData(const QOverrideHook hook) {
    setModelDataOverride = hook; 
  }

  void override_updateEditorGeometry(const QOverrideHook hook) {
    updateEditorGeometryOverride = hook; 
  }

  void override_editorEvent(const QOverrideHook hook) {
    editorEventOverride = hook; 
  }

  bool InheritedEditorEvent(QEvent * event, QAbstractItemModel * model, const QStyleOptionViewItem & option, const QModelIndex & index) {
    return QAbstractItemDelegate::editorEvent(event, model, option, index);
  };

private:

  //==================================================================================== 
  QOverrideHook sizeHintOverride;
  QOverrideHook paintOverride;
  QOverrideHook createEditorOverride;
  QOverrideHook setEditorDataOverride;
  QOverrideHook setModelDataOverride;
  QOverrideHook updateEditorGeometryOverride;
  QOverrideHook editorEventOverride;

  //==================================================================================== 
  QSize sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const {

    QSize size = QItemDelegate::sizeHint(option,index);

    if (sizeHintOverride.func) {

      typedef void (*func_type)(void *data, const QStyleOptionViewItemH option, const QModelIndexH index, QSizeH size);
      (*(func_type)sizeHintOverride.func)(sizeHintOverride.data, (const QStyleOptionViewItemH)&option, (const QModelIndexH)&index, (QSizeH) &size);

      } 

    return size;

  };

  //==================================================================================== 
  void paint(QPainter * painter, const QStyleOptionViewItem & option, const QModelIndex & index ) const {

    if (paintOverride.func) {
      typedef void (*func_type)(void *data, QPainterH painter, const QStyleOptionViewItemH option, const QModelIndexH index);
      (*(func_type)paintOverride.func)(paintOverride.data, (QPainterH) painter, (const QStyleOptionViewItemH)&option, (const QModelIndexH)&index);
      } 
  }


  QWidget* createEditor(QWidget * parent, const QStyleOptionViewItem &option, const QModelIndex &index) const {

    QWidget* widget = 0;
    if (createEditorOverride.func) {

      typedef void (*func_type)(void *data, QWidgetH parent, const QStyleOptionViewItemH option, const QModelIndexH index, QWidgetH w);
      (*(func_type)createEditorOverride.func)(createEditorOverride.data, (QWidgetH) parent, (const QStyleOptionViewItemH)&option, (const QModelIndexH)&index, (QWidgetH) &widget);
         if (widget == 0)
           widget = QItemDelegate::createEditor(parent, option, index);
      }
     else
        widget = QItemDelegate::createEditor(parent, option, index);


     return widget;

  };

  void setEditorData(QWidget * editor, const QModelIndex & index ) const {

    if (setEditorDataOverride.func) {

      typedef void (*func_type)(void *data, QWidgetH editor,  const QModelIndexH index);
      (*(func_type)setEditorDataOverride.func)(setEditorDataOverride.data, (QWidgetH) editor, (const QModelIndexH)&index);

      }
    else QItemDelegate::setEditorData(editor,index); 

  }

  void setModelData(QWidget * editor, QAbstractItemModel * model, const QModelIndex & index ) const {

    if (setModelDataOverride.func) {

      typedef void (*func_type)(void *data, QWidgetH editor,  QAbstractItemModelH model, const QModelIndexH index);
      (*(func_type)setModelDataOverride.func)(setModelDataOverride.data, (QWidgetH) editor, (QAbstractItemModelH) model, (const QModelIndexH)&index);

      } 
     else QItemDelegate::setModelData(editor,model,index);
  }

  void updateEditorGeometry(QWidget * parent, const QStyleOptionViewItem &option, const QModelIndex &index) const {

    if (updateEditorGeometryOverride.func) {

      typedef void (*func_type)(void *data, QWidgetH parent, const QStyleOptionViewItemH option, const QModelIndexH index);
      (*(func_type)updateEditorGeometryOverride.func)(updateEditorGeometryOverride.data, (QWidgetH) parent, (const QStyleOptionViewItemH)&option, (const QModelIndexH)&index);

      }
    else updateEditorGeometry(parent,option,index);
  };

  bool editorEvent(QEvent * event, QAbstractItemModel * model, const QStyleOptionViewItem & option, const QModelIndex & index) {

    bool result = false;

    if (editorEventOverride.func) {

      typedef void (*func_type)(void *data,const QEventH event, QAbstractItemModelH model, const QStyleOptionViewItemH option, const QModelIndexH index, bool * result);
      (*(func_type)editorEventOverride.func)(editorEventOverride.data, (const QEventH)event, (QAbstractItemModelH) model, (const QStyleOptionViewItemH)&option, (const QModelIndexH) &index, (bool *) &result);
      }
   else result = QAbstractItemDelegate::editorEvent(event, model, option, index);

   return result;

  };
};

#endif
