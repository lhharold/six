#ifndef __SIX_DYNLIBMGR_H_INCLUDE__
#define __SIX_DYNLIBMGR_H_INCLUDE__

namespace six {
	class DynLibMgr : public Singleton<DynLibMgr> {
  DECLARE_STATIC_LOG();
	public:
		DynLibMgr();
		virtual ~DynLibMgr();

		DynLib* load(const char* libName);

	protected:
		typedef Map<String, DynLib*> DynLibMap;
		DynLibMap mDynLibMap;
	};
}

#endif //__SIX_DYNLIBMGR_H_INCLUDE__