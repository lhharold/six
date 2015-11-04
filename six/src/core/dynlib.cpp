#include "core.h"

namespace six {
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