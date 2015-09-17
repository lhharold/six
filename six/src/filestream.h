#ifndef __SIX_FILESTREAM_H_INCLUDE__
#define __SIX_FILESTREAM_H_INCLUDE__

#include "datastream.h"

namespace six {
	class FileStream : public IDataStream {
	public:
		enum FILE_STREAM_ERROR_CODE {
			FILE_ERROR_NO_ERROR = 0,
			FILE_ERROR_FAILD_OPEN,
			FILE_ERROR_OPENED,
			FILE_ERROR_LENGTH_ERROR,
			FILE_ERROR_READ_OVER_END,
		};
		FileStream(const char* name);

		virtual bool open(const char* flags);
		virtual u32  read(void* buff, u32 size);
		virtual u32  write(void* buff, u32 size) {return 0;}
		virtual u32  readLine(char* buff, u32 maxCount, const char* delim = "\n");
		virtual u32  skipLine(const char* delim = "\n");
		virtual void skip(s32 count);
		virtual void seek(u32 pos);
		virtual u32  tell();
		virtual bool eof();
		virtual bool close();
	protected:
		void* mFile;
		u32   mError;
	};
}

#endif //__SIX_FILESTREAM_H_INCLUDE__