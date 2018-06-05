#include "core.h"

namespace six {
  DEFINE_STATIC_LOG(DynLib);
	DynLib::DynLib(const char* libName)
		: mLibName(libName)
	{

	}

	DynLib::~DynLib() {

	}

	void DynLib::load() {

	}

	void DynLib::unload() {

	}

	void* DynLib::getFunc(const char* funcName) {
		return NULL;
	}
}