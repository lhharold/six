#ifndef __SIX_DATASTREAM_H_INCLUDE__
#define __SIX_DATASTREAM_H_INCLUDE__

namespace six {
	class IDataStream {
	public:
		enum AccessMode {
			READ = 1,
			WRITE = 2,
		};
		IDataStream(u8 accessMode = READ) : mName(NULL), mSize(-1), mAccess(accessMode) {}
		IDataStream(const char* name, u8 accessMode = READ) : mName(name), mSize(-1), mAccess(accessMode) {
		}
		virtual ~DataStream() {
		}

		virtual bool open(const char* flags) = 0;
		virtual u32  read(void* buff, u32 size) = 0;
		virtual u32  write(void* buff, u32 size) {return 0;}
		virtual u32  readLine(char* buff, u32 maxCount, const char* delim = "\n");
		virtual u32  skipLine(const char* delim = "\n");
		virtual void skip(s32 count) = 0;
		virtual void seek(u32 pos) = 0;
		virtual u32  tell() = 0;
		virtual bool eof() = 0;
		virtual bool close() = 0;

		s32  size() {return mSize;}
		bool isReadable() const {return (mAccess & READ) != 0;}
		bool isWriteable() const {return (mAccess & WRITE} != 0;}

#if 0
		virtual void*	getHandle() = 0;
		virtual u32		getLength() = 0;
		virtual u32		getCurPos() = 0;
		virtual void	setCurPos(s32 pos) = 0;
		virtual void	seekOffset(s32 offset) = 0;

		virtual char	readChar() = 0;
		virtual short	readShort() = 0;
		virtual int		readInt() = 0;
		virtual s64		readInt64() = 0;
		virtual float	readFloat() = 0;
		virtual double	readDouble() = 0;
		virtual int		readBuffer(void* buffer, int size) = 0;
		virtual int		readString(char* str, int size) = 0;

		virtual void	writeChar(char val) = 0;
		virtual void	writeShort(short val) = 0;
		virtual void	writeInt(int val) = 0;
		virtual void	writeInt64(s64 val) = 0;
		virtual void	writeFloat(float val) = 0;
		virtual void	writeDouble(double val) = 0;
		virtual void	writeBuffer(void* buffer, int size) = 0;
		virtual void	writeBufferAlign(void* buffer, int size, int align) = 0;
		virtual void	writeString(const char* str) = 0;
#endif
	protected:
		String		mName;
		s32			mSize;
		u8			mAccess;
	};
}

#endif //__SIX_DATASTREAM_H_INCLUDE__