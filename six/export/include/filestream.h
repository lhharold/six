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
		FileStream(const char* name, s32 size);
		virtual ~FileStream();

		virtual bool	open(const char* flags);
		virtual void	close();
		virtual bool	readBool();
		virtual char	readChar();
		virtual short	readShort();
		virtual int		readInt();
		virtual s64		readInt64();
		virtual float	readFloat();
		virtual double	readDouble();
		virtual u32		readBuffer(void* buffer, int size);
		virtual u32		readString(char* str, u32 size);

		virtual void	writeBool(bool val);
		virtual void	writeChar(char val);
		virtual void	writeShort(short val);
		virtual void	writeInt(int val);
		virtual void	writeInt64(s64 val);
		virtual void	writeFloat(float val);
		virtual void	writeDouble(double val);
		virtual u32		writeBuffer(void* buffer, int size);
		virtual u32		writeBufferAlign(void* buffer, int size, u32 align);
		virtual u32		writeString(const char* str);

		virtual u32		readLine(char* buff, u32 maxCount, const char* delim = "\n");
		virtual u32		skipLine(const char* delim = "\n");
		virtual void	skip(s32 count);
		virtual void	seek(u32 pos);
		virtual u32		tell();

		int getError() {return mError;}
	protected:
		void determineAccess(const char* flags);

		void* mFile;
		int   mError;
	};
}

#endif //__SIX_FILESTREAM_H_INCLUDE__