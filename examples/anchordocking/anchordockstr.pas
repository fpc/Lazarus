{ Unit implementing anchor docking.

  Copyright (C) 2010 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit AnchorDockStr;

{$mode objfpc}{$H+}

interface

resourcestring
  adrsClose = 'Close';
  adrsUndock = 'Undock';
  adrsHeaderPosition = 'Header position';
  adrsEnlargeSide = 'Enlarge %s side';
  adrsMerge = 'Merge';
  adrsEnlarge = 'Enlarge';
  adrsAutomatically = 'Automatically';
  adrsLeft = 'left';
  adrsTop = 'top';
  adrsRight = 'right';
  adrsBottom = 'bottom';
  adrsLocked = 'Locked';
  adrsRequestedButCreated = '%s requested, but %s created';
  adrsDragAndDockC = 'Drag and dock %c';
  adrsMissingControlName = 'missing control name';
  adrsNotSupportedHasParent = 'Not supported: %s has parent %s';
  adrsAnchorNotFoundNodeAnchors = 'Anchor not found: Node=%s%s%s Anchors[%s]=%'
    +'s%s%s';
  adrsAnchorIsNotSplitterNodeAnchors = 'Anchor is not splitter: Node=%s%s%s '
    +'Anchors[%s]=%s%s%s';
  adrsAFreeSideOfASplitterMustNotBeAnchoredNodeTypeAncho = 'A free side of a '
    +'splitter must not be anchored: Node=%s%s%s Type=%s Anchors[%s]=%s%s%s';
  adrsAPageMustNotBeAnchoredNodeParentParentTypeAnchors = 'A page must not be '
    +'anchored: Node=%s%s%s Parent=%s ParentType=%s Anchors[%s]=%s%s%s';
  adrsAnchorToWrongSideOfSplitterNodeAnchors = 'Anchor to wrong side of '
    +'splitter: Node=%s%s%s Anchors[%s]=%s%s%s';
  adrsNoChildrenAllowedForNodeType = 'No children allowed for Node=%s%s%s '
    +'Type=%s';
  adrsEmptyName = 'Empty name: ';
  adrsDuplicateName = 'Duplicate name: ';

implementation

end.

