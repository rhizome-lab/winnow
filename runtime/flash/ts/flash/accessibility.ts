/**
 * flash.accessibility package — accessibility interfaces.
 */

/** AS3 `flash.accessibility.ISearchableText` — searchable text content. */
export abstract class ISearchableText {
  abstract get searchText(): string;
}

/** AS3 `flash.accessibility.ISimpleTextSelection` — text selection state. */
export abstract class ISimpleTextSelection {
  abstract get selectionActiveIndex(): number;
  abstract get selectionAnchorIndex(): number;
}
