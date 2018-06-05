#include "core.h"

#include "memorytracker.h"
#if OS_PLATFORM == OS_PLATFORM_WIN32
#   include <windows.h>
#	define OutputCString(str) ::OutputDebugStringA(str)
#	define OutputWString(str) ::OutputDebugStringW(str)
#else
#	define OutputCString(str) std::cerr << str
#	define OutputWString(str) std::cerr << str
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

namespace six {
#if MEMORY_TRACKER
	MemoryTracker& MemoryTracker::get() {
		static MemoryTracker tracker;
		return tracker;
	}
	void MemoryTracker::_recordAlloc(void* ptr, size_t sz, unsigned int pool, const char* file, size_t ln, const char* func) {
		_LOCK_AUTO_MUTEX

		ASSERT(mAllocations.find(ptr) == mAllocations.end() && "Double allocation with same address - "
			"this probably means you have a mismatched allocation / deallocation style, "
			"check if you're are using ALLOC_T / FREE and NEW_T / DELETE_T consistently");

		mAllocations[ptr] = Alloc(sz, pool, file, ln, func);
		if(pool >= mAllocationsByPool.size())
			mAllocationsByPool.resize(pool+1, 0);
		mAllocationsByPool[pool] += sz;
		mTotalAllocations += sz;
	}
	void MemoryTracker::_recordDealloc(void* ptr) {
		// deal cleanly with null pointers
		if (!ptr)
			return;

		_LOCK_AUTO_MUTEX

		AllocationMap::iterator i = mAllocations.find(ptr);
		ASSERT(i != mAllocations.end() && "Unable to locate allocation unit - "
			"this probably means you have a mismatched allocation / deallocation style, "
			"check if you're are using ALLOC_T / FREE and NEW_T / DELETE_T consistently");
		// update category stats
		mAllocationsByPool[i->second.pool] -= i->second.bytes;
		// global stats
		mTotalAllocations -= i->second.bytes;
		mAllocations.erase(i);
	}	
	size_t MemoryTracker::getTotalMemoryAllocated() const {
		return mTotalAllocations;
	}
	size_t MemoryTracker::getMemoryAllocatedForPool(unsigned int pool) const {
		return mAllocationsByPool[pool];
	}
	void MemoryTracker::reportLeaks() {		
    char os[1024];
    MEMSET(os, 0, sizeof(os));
		if (mAllocations.empty()) {
			STRCAT(os, "No memory leaks\n");
		} else {			
      SPRINTF(os, "Detected memory leaks !!!\nMemory: (%d) Allocation(s) with total %d bytes.\nDumping allocations ->\n",
        mAllocations.size(), mTotalAllocations);

      char fileError[256];
			for (AllocationMap::const_iterator i = mAllocations.begin(); i != mAllocations.end(); ++i) {
				const Alloc& alloc = i->second;
        if (!alloc.filename.empty()) {
          SPRINTF(fileError, "%s", alloc.filename);
          STRCAT(os, fileError);
        } else {
          STRCPY(fileError, "(unknown source):");
          STRCAT(os, fileError);
        }
        SPRINTF(fileError, "(%d) {%d bytes} function: %s\n", alloc.line, alloc.bytes, alloc.function);
        STRCAT(os, fileError);
			}			
      STRCAT(os, "\n");
		}

		if (mDumpToStdOut)		
      printf("%s", os);
		OutputCString(os);		

    FileSystem fs(".");
    IDataStream* ds = fs.open(mLeakFileName.c_str(), false);
    ds->writeBuffer(os, strlen(os));
    SAFE_DEL(ds);
	}
#endif // DEBUG_MODE	
	
}