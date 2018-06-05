#include "core.h"
#include "stringinterface.h"

namespace six {
  _STATIC_MUTEX_INSTANCE(StringInterface::sDictMutex);
  ArgDictionaryMap StringInterface::sDictionary;
}