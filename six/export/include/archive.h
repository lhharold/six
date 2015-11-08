#ifndef __SIX_ARCHIVE_H_INCLUDE__
#define __SIX_ARCHIVE_H_INCLUDE__

namespace six {
	class IDataStream;
	class Archive {
	public:
		Archive(const char* name) : mName(name), mReadonly(false) {}
		virtual ~Archive() {}
		virtual bool isCaseSensitive() const = 0;
		virtual void load() = 0;
		virtual void unload() = 0;
		virtual IDataStream* open(const char* filename, bool readonly = true) const = 0;
		virtual IDataStream* create(const char* filename) const {
			ASSERT("Archive::create  This archive does not support creation of files.");
			return NULL;
		}
		virtual void remove(const char* filename) const {
			ASSERT("Archive::remove  This archive does not support removal of files.");
		}
		virtual bool exists(const char* filename) = 0;
		virtual u32 getModifiedTime(const char* filename) = 0;

		const char* getName() const {return mName.c_str();}
		bool isReadOnly()const {return mReadonly;}
	protected:
		String mName;
		bool mReadonly;
	};
}

#endif //__SIX_ARCHIVE_H_INCLUDE__