#ifndef __SIX_DYNLIB_H_INCLUDE__
#define __SIX_DYNLIB_H_INCLUDE__

namespace six {
	class DynLib {
	public:
		DynLib(const char* libName);
		virtual ~DynLib();

		void load();
		void unload();
		const char* getName() {return mLibName.c_str();}
		void* getFunc(const char* funcName);
	protected:
		String mLibName;
	};
}
#endif //__SIX_DYNLIB_H_INCLUDE__