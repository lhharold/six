#include "define.h"
#include "filesystem.h"

namespace six {

    static bool is_reserved_dir (const char *fn) {
        return (fn [0] == '.' && (fn [1] == 0 || (fn [1] == '.' && fn [2] == 0)));
    }
    static bool is_absolute_path(const char* path) {
#if OS_PLATFORM  == OS_PLATFORM_WIN32
        if (isalpha(u8(path[0])) && path[1] == ':')
            return true;
#endif
        return path[0] == '/' || path[0] == '\\';
    }
    //static const char* concatenate_path(const String& base, const String& name) {
        //if (base.empty() || is_absolute_path(name.c_str()))
            //return name;
        //else
            //return base + '/' + name;
    //}

	FileSystem::FileSystem(const char* name) : Archive(name) {
	}

	FileSystem::~FileSystem() {
	}

	bool FileSystem::isCaseSensitive() const {
#if OS_PLATFORM == OS_PLATFORM_WIN32
		return false;
#else
		return true;
#endif
	}
	void FileSystem::load() {
	}
	void FileSystem::unload() {
	}
	IDataStream* FileSystem::open(const char* filename, bool readonly/* = true*/) const {
		return NULL;
	}
	u32 FileSystem::getModifiedTime(const char* filename) {
		return 0;
	}
	IDataStream* FileSystem::create(const char* filename) const {
		return NULL;
	}
	void FileSystem::remove(const char* filename) const {
	}
	bool FileSystem::exists(const char* filename) {
		return false;
	}

}