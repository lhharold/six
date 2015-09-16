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

        // test to see if this folder is writeable
		String testPath = concatenate_path(mName, "__testwrite.six");
		std::ofstream writeStream;
		writeStream.open(testPath.c_str());
		if (writeStream.fail())
			mReadOnly = true;
		else
		{
			mReadOnly = false;
			writeStream.close();
			::remove(testPath.c_str());
		}
#endif
	}
	void FileSystem::unload() {
	}
	IDataStream* FileSystem::open(const char* filename, bool readonly/* = true*/) const {
#if 0
		String full_path = concatenate_path(mName, filename);

		// Use filesystem to determine size 
		// (quicker than streaming to the end and back)
		struct stat tagStat;
		int ret = stat(full_path.c_str(), &tagStat);
		assert(ret == 0 && "Problem getting file size" );
        (void)ret;  // Silence warning

		// Always open in binary mode
		// Also, always include reading
		std::ios::openmode mode = std::ios::in | std::ios::binary;
		std::istream* baseStream = 0;
		std::ifstream* roStream = 0;
		std::fstream* rwStream = 0;

		if (!readonly && isReadonly()) {
			mode |= std::ios::out;
			rwStream = NEW_T(std::fstream, MEMCATEGORY_GENERAL)();
			rwStream->open(full_path.c_str(), mode);
			baseStream = rwStream;
		} else {
			roStream = NEW_T(std::ifstream, MEMCATEGORY_GENERAL)();
			roStream->open(full_path.c_str(), mode);
			baseStream = roStream;
		}

		// Should check ensure open succeeded, in case fail for some reason.
		if (baseStream->fail()) {
			DELETE_T(roStream, basic_ifstream, MEMCATEGORY_GENERAL);
			DELETE_T(rwStream, basic_fstream, MEMCATEGORY_GENERAL);
			EXCEPT(Exception::ERR_FILE_NOT_FOUND,
				"Cannot open file: " + filename,
				"FileSystemArchive::open");
		}

		/// Construct return stream, tell it to delete on destroy
		FileStream* stream = 0;
		if (rwStream) {
			// use the writeable stream 
			stream = NEW FileStream(filename, rwStream, (size_t)tagStat.st_size, true);
		} else {
			// read-only stream
			stream = NEW FileStream(filename, roStream, (size_t)tagStat.st_size, true);
		}
		return stream;
#else 
		return NULL;
#endif
	}
	u32 FileSystem::getModifiedTime(const char* filename) {
#if 0
		String full_path = concatenate_path(mName, filename);

		struct stat tagStat;
		bool ret = (stat(full_path.c_str(), &tagStat) == 0);

		if (ret) {
			return tagStat.st_mtime;
		}
		return 0;
#else
		return 0;
#endif
	}
	IDataStream* FileSystem::create(const char* filename) const {
#if 0
		if (isReadOnly()) {
			EXCEPT(Exception::ERR_INVALIDPARAMS, 
				"Cannot create a file in a read-only archive", 
				"FileSystemArchive::remove");
		}

		String full_path = concatenate_path(mName, filename);

		// Always open in binary mode
		// Also, always include reading
		std::ios::openmode mode = std::ios::out | std::ios::binary;
		std::fstream* rwStream = NEW_T(std::fstream, MEMCATEGORY_GENERAL)();
		rwStream->open(full_path.c_str(), mode);

		// Should check ensure open succeeded, in case fail for some reason.
		if (rwStream->fail())
		{
			DELETE_T(rwStream, basic_fstream, MEMCATEGORY_GENERAL);
			EXCEPT(Exception::ERR_FILE_NOT_FOUND,
				"Cannot open file: " + filename,
				"FileSystemArchive::create");
		}

		/// Construct return stream, tell it to delete on destroy
		FileStream* stream = NEW FileStream(filename, rwStream, 0, true);

		return stream;
#else
		return NULL;
#endif
	}
	void FileSystem::remove(const char* filename) const {
#if 0
		if (isReadonly()) {
			EXCEPT(Exception::ERR_INVALIDPARAMS, 
				"Cannot remove a file from a read-only archive", 
				"FileSystemArchive::remove");
		}
		String full_path = concatenate_path(mName, filename);
		::remove(full_path.c_str());
#endif
	}
	bool FileSystem::exists(const char* filename) {
#if 0
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
#else
		return false;
#endif
	}

}