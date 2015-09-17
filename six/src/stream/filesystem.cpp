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
    static String concatenate_path(const String& base, const String& name) {
        if (base.empty() || is_absolute_path(name.c_str()))
            return name;
        else
            return base + '/' + name;
    }

	FileSystem::FileSystem(const char* name) : Archive(name) {
	}

	FileSystem::~FileSystem() {
        unload();
	}

	bool FileSystem::isCaseSensitive() const {
#if OS_PLATFORM == OS_PLATFORM_WIN32
		return false;
#else
		return true;
#endif
	}
	void FileSystem::load() {
#if 0
		LOCK_AUTO_MUTEX
#endif
		//test to see if this folder is writeable
		String testPath = concatenate_path(mName, "__testwrite.six");
		FileStream fStream(testPath.c_str());
		if(fStream.open("wb")) {
			mReadonly = true;
		} else {
			mReadonly = false;
			fStream.close();
			FREMOVE(testPath.c_str());
		}
	}
	void FileSystem::unload() {
	}
	IDataStream* FileSystem::open(const char* filename, bool readonly/* = true*/) const {
		String full_path = concatenate_path(mName, filename);
		// Use filesystem to determine size 
		// (quicker than streaming to the end and back)
		struct stat tagStat;
		int ret = stat(full_path.c_str(), &tagStat);
		ASSERT(ret == 0 && "Problem getting file size" );

		// Always open in binary mode and reading
		const char* flags = "rb";
		FileStream* fStream = NEW FileStream(full_path.c_str(), (s32)tagStat.st_size);
		bool opened = false;
		if (!readonly && isReadOnly()) {
			opened = fStream->open("r+b");
		} else {
			opened = fStream->open("rb");
		}

		if(!opened) {
			ASSERT(0 && "FileSystem::open - Cannot open file");
			int error = fStream->getError();
			(void)error;
			SAFE_DEL(fStream);
		}
		return fStream;
	}
	u32 FileSystem::getModifiedTime(const char* filename) {
		String full_path = concatenate_path(mName, filename);
		struct stat tagStat;
		bool ret = (stat(full_path.c_str(), &tagStat) == 0);
		if (ret) {
			return tagStat.st_mtime;
		}
		return 0;
	}
	IDataStream* FileSystem::create(const char* filename) const {
		if (isReadOnly()) {
			ASSERT(0 && "FileSystem::create - Cannot create a file in a read-only archive");
			return NULL;
		}
		String full_path = concatenate_path(mName, filename);
		// Always open in binary mode and reading
		FileStream* fStream = NEW FileStream(full_path.c_str());
		bool opened = fStream->open("w+b");
		if (opened) {
			ASSERT(0 && "FileSystem::create - Cannot open file");
			SAFE_DEL(fStream);
		}
		return fStream;
	}
	void FileSystem::remove(const char* filename) const {
		if (isReadOnly()) {
			ASSERT(0 && "FileSystemArchive::remove - Cannot remove a file from a read-only archive");
			return;
		}
		String full_path = concatenate_path(mName, filename);
		FREMOVE(full_path.c_str());
	}
	bool FileSystem::exists(const char* filename) {
        String full_path = concatenate_path(mName, filename);
        struct stat tagStat;
        bool ret = (stat(full_path.c_str(), &tagStat) == 0);
		// stat will return true if the filename is absolute, but we need to check
		// the file is actually in this archive
        if (ret && is_absolute_path(filename.c_str())) {
			// only valid if full path starts with our base
#if OS_PLATFORM == OS_PLATFORM_WIN32
			// case insensitive on windows
			String lowerCaseName = mName;
			StringUtil::toLowerCase(lowerCaseName);
			ret = StringUtil::startsWith(full_path, lowerCaseName, true);
#else
			// case sensitive
			ret = tringUtil::startsWith(full_path, mName, false);
#endif
		}
		return ret;
	}

}