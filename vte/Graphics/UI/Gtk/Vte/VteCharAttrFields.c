#include "VteCharAttrFields.h"

gboolean getVteCharAttrUnderline(VteCharAttributes* vca) {
  return vca->underline;
}

gboolean getVteCharAttrStrikethrough(VteCharAttributes* vca) {
  return vca->strikethrough;
}

