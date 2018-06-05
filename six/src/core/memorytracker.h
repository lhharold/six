#ifndef __SIX_MEMORYTRACKER_H_INCLUDE__
#define __SIX_MEMORYTRACKER_H_INCLUDE__

// If using the GCC 3.1 C++ Std lib
#if COMPILER == COMPILER_GNUC && COMP_VER >= 310 && !defined(STLPORT)
// We need to define a hash function for void*
// For gcc 4.3 see http://gcc.gnu.org/gcc-4.3/changes.html
#   if COMP_VER >= 430
#       include <tr1/unordered_map>
#   else
#       include <ext/hash_map>
namespace __gnu_cxx {
	template <> struct hash< void* > {
		size_t operator()( void* const & ptr ) const {
			return (size_t)ptr;
		}
	};
}
#   endif
#endif

namespace six 
{
#if MEMORY_TRACKER
	class MemoryTracker {
	protected:
		_AUTO_MUTEX

		// Allocation record
		struct Alloc {
			u32 bytes;
			u32 pool;
			String filename;
			u32 line;
			String function;

			Alloc() :bytes(0), line(0) {}
			Alloc(u32 sz, u32 p, const char* file, u32 ln, const char* func) 
        :bytes(sz), pool(p), line(ln)
      {
				if (file)
					filename = file;
				if (func)
					function = func;
			}
		};

		String mLeakFileName;
		bool mDumpToStdOut;
		typedef Map<void*, Alloc> AllocationMap;
		AllocationMap mAllocations;

		u32 mTotalAllocations;
		typedef Vector<u32> AllocationsByPool;
		AllocationsByPool mAllocationsByPool;

		void reportLeaks();

		// protected ctor
		MemoryTracker() : mLeakFileName("Leaks.log"), mDumpToStdOut(true), mTotalAllocations(0) { }
	public:
		void setReportFileName(const String& name) { mLeakFileName = name; }
		const String& getReportFileName() const { return mLeakFileName; }
		void setReportToStdOut(bool rep) { mDumpToStdOut = rep; }
		bool getReportToStdOut() const { return mDumpToStdOut; }

		u32 getTotalMemoryAllocated() const;
		u32 getMemoryAllocatedForPool(unsigned int pool) const;
		void _recordAlloc(void* ptr, size_t sz, unsigned int pool = 0, const char* file = 0, size_t ln = 0, const char* func = 0);
		void _recordDealloc(void* ptr);

		~MemoryTracker() {
			reportLeaks();
		}
		static MemoryTracker& get();
	};
#endif
}
#endif //__SIX_MEMORYTRACKER_H_INCLUDE__