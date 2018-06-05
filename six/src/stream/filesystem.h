#ifndef __SIX_FILESYSTEM_H_INCLUDE__
#define __SIX_FILESYSTEM_H_INCLUDE__

#include "archive.h"

namespace six {
	class FileSystem : public Archive {
	public:
		FileSystem(const char* name);
		virtual ~FileSystem();

		virtual bool isCaseSensitive() const;
		virtual void load();
		virtual void unload();
		virtual IDataStream* open(const char* filename, bool readonly = true) const;
		virtual u32 getModifiedTime(const char* filename);
		virtual IDataStream* create(const char* filename) const;
		virtual void remove(const char* filename) const;
		virtual bool exists(const char* filename);
	};
}

#endif //__SIX_FILESYSTEM_H_INCLUDE__