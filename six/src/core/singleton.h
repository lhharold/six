#ifndef __SIX_SINGLETON_H_INCLUDE__
#define __SIX_SINGLETON_H_INCLUDE__

namespace six {
	template<typename T>
	class Singleton {
	private:
		Singleton(const Singleton<T> &);
		Singleton& operator=(const Singleton<T>& );
	protected:
		static T* sInstance;

	public:
		Singleton() {
			ASSERT(sInstance == NULL);
		    sInstance = static_cast<T*>(this);
		}
		virtual ~Singleton() {
			ASSERT(sInstance != NULL);
			sInstance = NULL;
		}
		static T& get() {
			ASSERT(sInstance != NULL);
			return *sInstance;
		}
		static T* getPtr() {
			ASSERT(sInstance != NULL);
			return sInstance;
		}
	};
}

#endif //__SIX_SINGLETON_H_INCLUDE__