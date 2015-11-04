#include "core.h"
#include "dynlibmgr.h"

namespace six {
	template<> DynLibMgr* Singleton<DynLibMgr>::sInstance = NULL;
	DynLibMgr::DynLibMgr() {

	}

	DynLibMgr::~DynLibMgr() {

	}

	DynLib* DynLibMgr::load(const char* libName) {
		DynLibMap::iterator itr = mDynLibMap.find(libName);
		if(itr != mDynLibMap.end()) {
			return itr->second;
		}
		DynLib* lib = NEW DynLib(libName);
		lib->load();
		mDynLibMap[libName] = lib;
		return lib;
	}
}