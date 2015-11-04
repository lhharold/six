#ifndef __SIX_DYNLIBMGR_H_INCLUDE__
#define __SIX_DYNLIBMGR_H_INCLUDE__

namespace six {
	class DynLibMgr : public Singleton<DynLibMgr> {
	public:
		DynLibMgr();
		virtual ~DynLibMgr();

		DynLib* load(const char* libName);

	protected:
		typename Map<String, DynLib*> DynLibMap;
		DynLibMap mDynLibMap;
	};
}

#endif //__SIX_DYNLIBMGR_H_INCLUDE__