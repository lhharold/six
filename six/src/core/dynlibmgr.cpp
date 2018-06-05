#include "core.h"

namespace six {
	template<> DynLibMgr* Singleton<DynLibMgr>::sInstance = NULL;
  DEFINE_STATIC_LOG(DynLibMgr);
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