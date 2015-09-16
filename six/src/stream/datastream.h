#ifndef __SIX_DATASTREAM_H_INCLUDE__
#define __SIX_DATASTREAM_H_INCLUDE__
#include "define.h"
namespace six {
	class IDataStream {
	public:
		enum AccessMode {
			READ = 1,
			WRITE = 2,
		};
		IDataStream(u8 accessMode = READ) : mName(NULL), mSize(0), mAccess(accessMode) {}
		IDataStream(const char* name, u8 accessMode = READ) : mSize(0), mAccess(accessMode) {
			u32 len = strlen(name);
			mName = NEW char[len+1];
			STRCPY(mName, name);
		}
		virtual ~DataStream() {
			SAFE_DEL(mName);
		}

		virtual u32  read(void* buff, u32 size) = 0;
		virtual u32  write(void* buff, u32 size) {return 9;}
		virtual u32  readLine(char* buff, u32 maxCount, const char* delim = "\n");
		virtual u32  skipLine(const char* delim = "\n");
		virtual void skip(s32 count) = 0;
		virtual void seek(u32 pos) = 0;
		virtual u32  tell() = 0;
		virtual bool eof() = 0;
		virtual bool close() = 0;

		u32  size() {return mSize;}
		bool isReadable() const {return (mAccess & READ) != 0;}
		bool isWriteable() const {return (mAccess & WRITE} != 0;}
	protected:
		const char* mName;
		u32			mSize;
		u8			mAccess;
	};
}

#endif //__SIX_DATASTREAM_H_INCLUDE__