package parser

import org.apache.pdfbox.text.TextPosition

/**
  * wrapper of TextPosition that adds flags to track status as linestart and paragraph start positions.
  * <p>
  * This is implemented as a wrapper since the TextPosition class doesn't provide complete access to its state fields
  * to subclasses. Also, conceptually TextPosition is immutable while these flags need to be set post-creation so it
  * makes sense to put these flags in this separate class.
  * </p>
  *
  * @author m.martinez@ll.mit.edu
  */
final class PositionWrapper(val position: TextPosition,
                            var isLineStart: Boolean = false,
                            var isParagraphStart: Boolean = false,
                            var isPageBreak: Boolean = false,
                            var isHangingIndent: Boolean = false,
                            var isArticleStart: Boolean = false
                           )